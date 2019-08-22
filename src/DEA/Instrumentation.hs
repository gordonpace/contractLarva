-- Copyright 2017 Gordon J. Pace and Joshua Ellul
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module DEA.Instrumentation (instrumentSpecification) where


import Control.Monad hiding (guard)
import Text.Parsec hiding (State, label)
import Text.Parsec.String

import Data.List
import Data.Maybe
import Data.Either

import DEA.DEA
import DEA.Parsing
import Solidity

instrumentContractSpecification :: ContractSpecification -> Instrumentation
instrumentContractSpecification monitor =
  -- (i)  Rename contract to LARVA_contract
    renameContract (contract, contract') |>

    --rename old constructor to new contractname if using old style constructor (will only have effect if this is present)
    renameFunctionInContract contract' (contract, contract') |>

  -- (ii) Add LARVA_Status handlers

    addContractParts contract' (
      map parser'
        [ 
          -- Enumerated type to keep track whether contract is enabled or not.
          "enum LARVA_STATUS { ENABLED, DISABLED }"
          -- Functionality to enable and disable the original contract
          , "function LARVA_EnableContract() private { LARVA_Status = LARVA_STATUS.ENABLED; }"
          , "function LARVA_DisableContract() private { LARVA_Status = LARVA_STATUS.DISABLED; }"
            -- Modifier to ensure that the contract has been enabled
          , "modifier LARVA_ContractIsEnabled { require(LARVA_Status == LARVA_STATUS.ENABLED); _; }"
        ]) |>  
    
    -- Add the modifier to check that the contract is enabled to all functions (except to any old style constructors)
    addTopModifierToAllButTheseFunctionInContract
    contract' [contract'] (Identifier "LARVA_ContractIsEnabled", ExpressionList []) |>


  -- (ii) Add monitor initialisation and enabling code

    --If a constructor is not defined in the smart contract 
    --  then if the monitor does not require any initialisation logic
    --          then simply set the default value of the monitor flag (LARVA_Status) to enabled (avoiding the creation of a constructor),
    --          otherwise create a new constructor in a style compatible with code's pragma version
    --  otherwise do nothing
    (\x -> if(not $ constructorIsDefinedInContract contract' x)
              then (if initialisation monitor == Block []
                    then addContractParts contract' 
                          (map parser' [
                            -- LARVA_status keeps track of the current status of the contract
                            "LARVA_STATUS private LARVA_Status = LARVA_STATUS.ENABLED;"
                          ]) x

                    else if (useNewStyleConstructor x)
                          then (addContractParts contract' 
                                            [ (ContractPartConstructorDefinition
                                                (ParameterList [])
                                                [FunctionDefinitionTagPublic]
                                                Nothing
                                              )] x)
                          else (addContractParts contract' 
                                          [ (ContractPartFunctionDefinition
                                              (Just contract') (ParameterList [])
                                              [FunctionDefinitionTagPublic]
                                              Nothing
                                              Nothing
                                            )] x)
                    )
              else x

              
    ) |>

    --If a constructor is defined in the smart contract 
    --   (we specify this case separately from the previous code since we want it to run in the case we are creating a constructor ourselves)
    --  then set the default value of the monitor flag (LARVA_Status) to disabled and create a constructor modifier that initialises the monitor and enables it,
    --  otherwise do nothing
    (\x -> if constructorIsDefinedInContract contract' x
            then (addContractParts contract' 
                  (map parser' [
                    -- LARVA_status keeps track of the current status of the contract
                    "LARVA_STATUS private LARVA_Status = LARVA_STATUS.DISABLED;"
                    -- Modifier to be added to the old constructor to initialise and start monitor on constructor running
                    , "modifier LARVA_Constructor {"++(display $ initialisation monitor)++"LARVA_Status = LARVA_STATUS.ENABLED;\n_;}"
                  ]) |>

                  addTopModifierToContractConstructor
                  contract' (Identifier "LARVA_Constructor", ExpressionList [])
                  ) x
            else x
      )

      |>



  -- (iii) Add declarations, reparation, satisfaction functions of monitored contract
    addContractParts contract' (
      [(ContractPartFunctionDefinition
          (Just $ Identifier "LARVA_reparation") (ParameterList [])
          [FunctionDefinitionTagPrivate]
          Nothing
          (Just $ reparation monitor)
        )
      , (ContractPartFunctionDefinition
          (Just $ Identifier "LARVA_satisfaction") (ParameterList [])
          [FunctionDefinitionTagPrivate]
          Nothing
          (Just $ satisfaction monitor)
        )
      ] ++ declarations monitor
    ) |>

  -- (iv) Add setters for relevant variables
    foldl (.) id
    [ defineAndUseSetterFunctionForVariableInContract contract' v
          (Identifier ("LARVA_set_"++vn++"_pre"), Identifier ("LARVA_set_"++vn++"_post"))
    | v <- getVariablesFromContractSpecification monitor
    , let vn = display v
    ] |>

  -- Start instrumenting the DEAs
  -- (v) Add variables to store state of each DEA: LARVA_STATE_n, all initialised to 0.
    addContractParts contract'
      [ parser' ("int8 "++larva_state_variable n++" = 0;") | n <- [1..deaCount] ] |>

  -- (vi) Create modifiers to catch events and change state + attach modifiers to relevant functions
    foldl (|>) id [ instrumentForDEA contract' (n,d) | (n,d) <- zip [1..] ds ]

  where
    contract = contractName monitor
    contract' = Identifier $ "LARVA_" ++ display contract

    ds = deas monitor
    deaCount = length ds

    larva_state_variable n = "LARVA_STATE_"++show n

    parser' = fromRight undefined . parse parser ""

    f |> g = g . f

    instrumentForDEA :: ContractName -> (Int, DEA) -> Instrumentation
    instrumentForDEA contractName (deaNumber, dea) code =
        foldl (|>) id (
          map instrumentForEvent $
            -- this returns (event, transitions which trigger on this event for this automaton)
            map (\ets -> (fst $ head ets, map snd ets)) $
              -- this groups the transitions by event
              groupBy (\(e,_) (e',_) -> sameEvent e e') $
                sortOn fst [ (event $ label t, t) | t <- transitions dea ]
        ) code
      where
        sameEvent (UponEntry f) (UponEntry f') = f==f'
        sameEvent (UponExit f) (UponExit f') = f==f'
        sameEvent (VariableAssignment v _) (VariableAssignment v' _) = v==v'
        sameEvent _ _ = False

        modifierCode :: String -> Maybe ParameterList -> Bool -> [Transition] -> String
        modifierCode modifierName mps before ts =
          unlines $
            [ "modifier "++modifierName++maybe "" display mps++" {"] ++
            (if before then [] else ["_;"]) ++
            [ "if (("++larva_state_variable deaNumber++" == "++stateNumber (src t)++")"++
              maybe "" (\cond -> " && ("++display cond++")") (guard gcl) ++
              maybe "" (\cond -> " && ("++display cond++")") condition ++
              ") { "++larva_state_variable deaNumber++" = "++stateNumber (dst t)++";"++
              display (action gcl)++reparationCode++satisfactionCode++"} else {"
            | t <- ts, let gcl = label t
            , let condition = variableAssignmentExpression gcl
            , let reparationCode = if dst t `elem` badStates dea then " LARVA_reparation(); " else ""
            , let satisfactionCode = if dst t `elem` acceptanceStates dea then " LARVA_satisfaction(); " else ""
            ] ++
            [ "   "++ replicate (length ts) '}' ] ++
            (if before then ["_;"] else []) ++
            [ "}" ]
          where
            variableAssignmentExpression gcl =
              case event gcl of
                VariableAssignment _ e -> e
                _                      -> Nothing

        nameModifier :: Event -> String
        nameModifier (UponEntry fc) =
          "LARVA_DEA_"++show deaNumber++"_handle_before_"++display (functionName fc)++
          maybe "__no_parameters" (\ps -> "__parameters_"++intercalate "_" (map display $ fromUntypedParameterList ps)) (parametersPassed fc)
        nameModifier (UponExit fc) =
          "LARVA_DEA_"++show deaNumber++"_handle_after_"++display (functionName fc)++
          maybe "__no_parameters" (\ps -> "__parameters_"++intercalate "_" (map display $ fromUntypedParameterList ps)) (parametersPassed fc)
        nameModifier (VariableAssignment vn _) =
          "LARVA_DEA_"++show deaNumber++"_handle_after_assignment_"++display vn


        instrumentForEvent :: (Event, [Transition]) -> Instrumentation
        instrumentForEvent (e@(UponEntry functionCall), ts) =
          let function = functionName functionCall

              functionTypedParameters = getFunctionParameters contractName function code
              functionParameters =
                ExpressionList $
                  maybe []
                    (const $ map (Literal . PrimaryExpressionIdentifier) $
                      fromUntypedParameterList $ untypeParameterList functionTypedParameters)
                        eventUntypedParameters

              eventUntypedParameters = parametersPassed functionCall
              eventTypedParameters =
                maybe Nothing (\ps -> Just (typeParameterList ps functionTypedParameters)) eventUntypedParameters

              modifierName =  nameModifier e
          in  -- Add modifier to function
              addTopModifierToFunctionInContract contractName function
                (Identifier modifierName, functionParameters) |> (
              -- Define modifier to trigger transitions
              addContractPart contractName $ parser' $ modifierCode modifierName eventTypedParameters True ts
              )
        instrumentForEvent (e@(UponExit functionCall), ts) =
          let function = functionName functionCall

              functionTypedParameters = getFunctionParameters contractName function code
              functionParameters =
                ExpressionList $
                  maybe []
                    (const $ map (Literal . PrimaryExpressionIdentifier) $
                      fromUntypedParameterList $ untypeParameterList functionTypedParameters)
                        eventUntypedParameters

              eventUntypedParameters = parametersPassed functionCall
              eventTypedParameters =
                maybe Nothing (\ps -> Just (typeParameterList ps functionTypedParameters)) eventUntypedParameters

              modifierName = nameModifier e
          in -- Add modifier to function
             addTopModifierToFunctionInContract contractName function
               (Identifier modifierName, functionParameters) |> (
             -- Define modifier to trigger transitions
             addContractPart contractName $ parser' $ modifierCode modifierName eventTypedParameters True ts
             )
        instrumentForEvent (e@(VariableAssignment variableName _), ts) =
          let modifierName =  nameModifier e
          in  -- Define modifier to trigger transitions
              (addContractPart contractName $ parser' $ modifierCode modifierName Nothing False ts) |>
              -- Add modifier to setter functions
              addTopModifierToFunctionsInContract contractName
                [ Identifier ("LARVA_set_"++display variableName++"_pre")
                , Identifier ("LARVA_set_"++display variableName++"_post")
                ]
                (Identifier modifierName, ExpressionList [])


        stateNumber stateName = show $ fromJust $ elemIndex stateName ss
          where
            s0 = initialState dea
            ss = s0: (allStates dea \\ [s0])


instrumentSpecification :: Specification -> Instrumentation
instrumentSpecification specification =
  foldl (\f c -> instrumentContractSpecification c . f) id (contractSpecifications specification)

