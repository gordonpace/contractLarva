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

module DEA.Instrumentation (instrumentSpecification, warningsSpecification) where


import Control.Monad hiding (guard)
import Text.Parsec hiding (State, label)
import Text.Parsec.String

import Data.List
import Data.Maybe
import Data.Either

import DEA.DEA
import DEA.Parsing
import Solidity

import Debug.Trace

instrumentContractSpecification :: ContractSpecification -> Bool -> Instrumentation
instrumentContractSpecification monitor notInlined =
  -- (i)  Rename contract to LARVA_contract
    renameContract (contract, contract') |>
        --rename old-style constructor to new contract name
    renameFunctionInContract contract' (contract, contract') |>

  -- (ii) Add LARVA_Status handlers

    -- Add the modifier to check that the contract is enabled to all functions (except to any old style constructors)
    addTopModifierToAllButTheseFunctionInContract
      contract' [contract'] (Identifier "LARVA_ContractIsEnabled", ExpressionList []) |>

    addContractParts contract' (
      map parser'
        [ 
          -- Enumerated type to keep track whether contract is enabled or not.
          "enum LARVA_STATUS { RUNNING, STOPPED }"
          -- Functionality to enable and disable the original contract
          , "function LARVA_EnableContract() private { LARVA_Status = LARVA_STATUS.RUNNING; }"
          , "function LARVA_DisableContract() private { LARVA_Status = LARVA_STATUS.STOPPED; }"
          -- LARVA_status keeps track of the current status of the contract
          ,"LARVA_STATUS private LARVA_Status = LARVA_STATUS.STOPPED;"
          -- Modifier to ensure that the contract has been enabled
          , "modifier LARVA_ContractIsEnabled { require(LARVA_Status == LARVA_STATUS.RUNNING); _; }"        ]) |>  
    
  --(iii)-Add constructor if constructor not defined and inlining intended
    (\x -> if not inliningFlag
            then x
            else if constructorIsDefinedInContract contract' x
                  then x
                  else if useNewStyleConstructor x
                      then addContractParts contract' 
                                        [ ContractPartConstructorDefinition
                                            (ParameterList [])
                                            [FunctionDefinitionTagPublic]
                                            (Just $ Block [])
                                            ] x
                      else addContractParts contract' 
                                      [ ContractPartFunctionDefinition
                                          (Just contract') (ParameterList [])
                                          [FunctionDefinitionTagPublic]
                                          Nothing
                                          (Just $ Block [])
                                        ] x)
     |>

  -- (iv) Add reparation and satisfaction functions if they are not empty and there are respectively bad and acceptance states,
  --       and the declaration code of monitored contract
    addContractParts contract' (
        [ContractPartFunctionDefinition
                  (Just $ Identifier "LARVA_reparation") (ParameterList [])
                  [FunctionDefinitionTagPrivate]
                  Nothing
                  (Just $ reparation monitor)
                | reparation monitor /= Block [] && [s | dea <- deas monitor, s <- badStates dea] /= []]
        ++ [ContractPartFunctionDefinition
                  (Just $ Identifier "LARVA_satisfaction") (ParameterList [])
                  [FunctionDefinitionTagPrivate]
                  Nothing
                  (Just $ satisfaction monitor)
                | satisfaction monitor /= Block [] && [s | dea <- deas monitor, s <- acceptanceStates dea] /= []]
          ++ declarations monitor
    ) |>

  -- (v.0) Add setters for transfer and selfdestruct
    defineAndUseSetterFunctionForTransferInContract contract' |>
    defineAndUseSetterFunctionForSelfDestructInContract contract' |>
    
  -- (v) Add setters for relevant variables
    foldl (.) id
    [ defineAndUseSetterFunctionForVariableInContract contract' v
          (Identifier ("LARVA_set_"++vn++"_pre"), Identifier ("LARVA_set_"++vn++"_post"))
    | v <- getVariablesFromContractSpecification monitor
    , let vn = display v
    ] |>


  -- Start instrumenting the DEAs
  -- (vi) Add variables to store state of each DEA: LARVA_STATE_n, all initialised to 0.
    addContractParts contract'
      [ parser' ("int8 "++larva_state_variable n++" = 0;") | n <- [1..deaCount] ] |>

  -- (vii) Create modifiers to catch events and change state + attach modifiers to relevant functions
    foldl (|>) id [ instrumentForDEA contract' (n,d) | (n,d) <- zip [1..] ds ] 
    |>

  -- (viii) Inline or not the contract initialisation code
      if not inliningFlag && not (initialisation monitor == (Block []))
        then addContractParts contract'
                (map parser' [
                  -- Non-inlined monitor initilisation function
                "function LARVA_Constructor() public {" ++ display (initialisation monitor)  ++  " }"])
        else (addContractParts contract' 
                (map parser' [
                  -- Inlined monitor initilisation function as modifier
                "modifier LARVA_Constructor {" ++ "_; "  ++ display (initialisation monitor)  ++  " }"])
                |>
              addTopModifierToContractConstructor contract' (Identifier "LARVA_Constructor", ExpressionList []))

  where
    inliningFlag = not notInlined
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
        map (instrumentForEvent . 
              (\ ets -> (fst $ head ets, map snd ets)))
            (groupBy (\ (e, _) (e', _) -> sameEvent e e') $
              sortOn fst [(event $ label t, t) | t <- transitions dea])
      ) code
      where
        -- distributeNameMatch :: [[(Event, Transition)]] -> [[(Event, Transition)]]
        -- distributeNameMatch [e,ts]:ets = 
        -- distributeNameMatchForEvent :: Event -> [[(Event, Transition)]] -> [[(Event, Transition)]]
        -- distributeNameMatchForEvent (UponEntry f) ets = if parametersPassed f = Nothing 
        --                                                   then ets
        --                                                   else 
        
        -- pushNothingToBack ((UponEntry f, t):es) = if parametersPassed f == Nothing
        --                                         then (pushNothingToBack es) ++ [(UponEntry f, t)]
        --                                         else ((UponEntry f, t):es)
        -- pushNothingToBack ((UponExit f,t):es) = if parametersPassed f == Nothing
        --                                         then (pushNothingToBack es) ++ [(UponExit f, t)]
        --                                         else ((UponExit f, t):es)
        -- pushNothingToBack [] = []

        sameEvent (UponEntry f) (UponEntry f') = f==f'
        sameEvent (UponExit f) (UponExit f') =  f==f'
        sameEvent (BeforeTransfer) (BeforeTransfer) = True
        sameEvent (AfterTransfer) (AfterTransfer) = True
        sameEvent (BeforeSelfDestruct) (BeforeSelfDestruct) = True
        sameEvent (AfterSelfDestruct) (AfterSelfDestruct) = True
        sameEvent (VariableAssignment v _) (VariableAssignment v' _) = v==v'
        sameEvent _ _ = False

        modifierCode :: SolidityCode -> ContractName -> String -> Maybe ParameterList -> Bool -> [Transition] -> String
        modifierCode code contract modifierName mps before ts =
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
            , let reparationCode = if dst t `elem` badStates dea && functionIsDefinedInContract contract (Identifier "LARVA_reparation") code then " LARVA_reparation(); " else ""
            , let satisfactionCode = if dst t `elem` acceptanceStates dea && functionIsDefinedInContract contract (Identifier "LARVA_satisfaction") code then " LARVA_satisfaction(); " else ""
            ] ++
            [ "   "++ replicate (length ts) '}' ] ++
            ["_;" | before] ++
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
        nameModifier (BeforeTransfer) =
          "LARVA_DEA_"++show deaNumber++"_handle_before_transfer"
        nameModifier (AfterTransfer) =
          "LARVA_DEA_"++show deaNumber++"_handle_after_transfer"
        nameModifier (BeforeSelfDestruct) =
          "LARVA_DEA_"++show deaNumber++"_handle_before_selfdestruct"
        nameModifier (AfterSelfDestruct) =
          "LARVA_DEA_"++show deaNumber++"_handle_after_selfdestruct"
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
                maybe Nothing (\ps -> Just (addMemoryLocationToParametersList $ typeParameterList ps functionTypedParameters)) eventUntypedParameters

              modifierName =  nameModifier e
          in  -- Add modifier to function
              addTopModifierToFunctionInContract contractName function
                (Identifier modifierName, functionParameters) |> (
              -- Define modifier to trigger transitions
                \x -> (addContractPart contractName $ parser' $ modifierCode x contractName modifierName eventTypedParameters True ts) x
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
                maybe Nothing (\ps -> Just (addMemoryLocationToParametersList $ typeParameterList ps functionTypedParameters)) eventUntypedParameters

              modifierName = nameModifier e
          in -- Add modifier to function
             addTopModifierToFunctionInContract contractName function
               (Identifier modifierName, functionParameters) |> (
             -- Define modifier to trigger transitions
              \x -> (addContractPart contractName $ parser' $ modifierCode x contractName modifierName eventTypedParameters False ts) x
             )
        instrumentForEvent (e@(VariableAssignment variableName _), ts) =
          let modifierName =  nameModifier e
          in  -- Define modifier to trigger transitions
              (\x -> (addContractPart contractName $ parser' $ modifierCode x contractName modifierName Nothing False ts) x)|>
              -- Add modifier to setter functions
              addTopModifierToFunctionsInContract contractName
                [ Identifier ("LARVA_set_"++display variableName++"_pre")
                , Identifier ("LARVA_set_"++display variableName++"_post")
                ]
                (Identifier modifierName, ExpressionList [])
        instrumentForEvent (e@(BeforeTransfer), ts) =
          let modifierName =  nameModifier e
          in  -- Define modifier to trigger transitions
              (\x -> (addContractPart contractName $ parser' $ modifierCode x contractName modifierName Nothing False ts) x)|>
              -- Add modifier to setter functions
              addTopModifierToFunctionsInContract contractName
                [ Identifier ("LARVA_transfer")
                ]
                (Identifier modifierName, ExpressionList [])
        instrumentForEvent (e@(AfterTransfer), ts) =
          let modifierName =  nameModifier e
          in  -- Define modifier to trigger transitions
              (\x -> (addContractPart contractName $ parser' $ modifierCode x contractName modifierName Nothing False ts) x)|>
              -- Add modifier to setter functions
              addTopModifierToFunctionsInContract contractName
                [ Identifier ("LARVA_transfer")
                ]
                (Identifier modifierName, ExpressionList [])
        instrumentForEvent (e@(BeforeSelfDestruct), ts) =
          let modifierName =  nameModifier e
          in  -- Define modifier to trigger transitions
              (\x -> (addContractPart contractName $ parser' $ modifierCode x contractName modifierName Nothing False ts) x)|>
              -- Add modifier to setter functions
              addTopModifierToFunctionsInContract contractName
                [ Identifier ("LARVA_selfdestruct")
                ]
                (Identifier modifierName, ExpressionList [])
        instrumentForEvent (e@(AfterSelfDestruct), ts) =
          let modifierName =  nameModifier e
          in  -- Define modifier to trigger transitions
              (\x -> (addContractPart contractName $ parser' $ modifierCode x contractName modifierName Nothing False ts) x)|>
              -- Add modifier to setter functions
              addTopModifierToFunctionsInContract contractName
                [ Identifier ("LARVA_selfdestruct")
                ]
                (Identifier modifierName, ExpressionList [])


        stateNumber stateName = show $ fromJust $ elemIndex stateName ss
          where
            s0 = initialState dea
            ss = s0: (allStates dea \\ [s0])


instrumentSpecification :: Specification -> Bool -> Instrumentation
instrumentSpecification specification inliningFlag =
  foldl (\f c -> instrumentContractSpecification c inliningFlag . f) id (contractSpecifications specification)

-----------------------

-- IfStatement Expression Statement (Maybe Statement)
--   | WhileStatement Expression Statement
--   | InlineAssemblyStatement (Maybe StringLiteral) InlineAssemblyBlock
--   | ForStatement (Maybe Statement, Maybe Expression, Maybe Expression) Statement
--   | BlockStatement Block

--   | DoWhileStatement Statement Expression
--   | PlaceholderStatement
--   | Continue
--   | Break
--   | Return (Maybe Expression)
--   | Throw
--   | EmitStatement Expression

--   | SimpleStatementExpression Expression
--   | SimpleStatementVariableList IdentifierList (Maybe Expression)
--  -- | SimpleStatementVariableDeclaration VariableDeclaration (Maybe Expression)
--   | SimpleStatementVariableDeclarationList [Maybe VariableDeclaration] [Expression]
--   | SimpleStatementVariableAssignmentList [Maybe Identifier] [Expression]

monitorDeclarationsHasCustomEnablingLogic :: ContractSpecification -> Bool
monitorDeclarationsHasCustomEnablingLogic monitor = ["" | (ContractPartFunctionDefinition _ _ _ _ (Just b)) <- declarations monitor, containsCallToFunction (Identifier "LARVA_EnableContract") (BlockStatement b)] /= []

monitorInitialisationHasCustomEnablingLogic :: ContractSpecification -> Bool
monitorInitialisationHasCustomEnablingLogic monitor = containsCallToFunction (Identifier "LARVA_EnableContract") (BlockStatement (initialisation monitor))

containsCallToFunction :: FunctionName -> Statement -> Bool
containsCallToFunction f (IfStatement _ stmt Nothing) = containsCallToFunction f stmt
containsCallToFunction f (IfStatement _ stmt (Just stmtt)) = containsCallToFunction f stmt || containsCallToFunction f stmtt
containsCallToFunction f (WhileStatement _ stmt) = containsCallToFunction f stmt
containsCallToFunction f (DoWhileStatement stmt _) = containsCallToFunction f stmt
containsCallToFunction f (ForStatement (Nothing, _, _) stmt) = containsCallToFunction f stmt
containsCallToFunction f (ForStatement (Just stmtt , _, _) stmt) = containsCallToFunction f stmt || containsCallToFunction f stmtt
containsCallToFunction f (SimpleStatementExpression expr) = exprContainsCallToFunction f expr
containsCallToFunction f (BlockStatement (Block (s:stmts))) = containsCallToFunction f s || containsCallToFunction f (BlockStatement (Block stmts))
containsCallToFunction _ _ = False

exprContainsCallToFunction :: FunctionName -> Expression -> Bool
exprContainsCallToFunction f (Unary _ ex) = exprContainsCallToFunction f ex
exprContainsCallToFunction f (Binary _ e1 e2) = exprContainsCallToFunction f e1 || exprContainsCallToFunction f e2
exprContainsCallToFunction f (Ternary _ e1 e2 e3) = exprContainsCallToFunction f e1 || exprContainsCallToFunction f e2 || exprContainsCallToFunction f e3
exprContainsCallToFunction f (FunctionCallNameValueList e1 Nothing) = exprContainsCallToFunction f e1
exprContainsCallToFunction f (FunctionCallNameValueList e1 (Just (NameValueList nameValueList))) = exprContainsCallToFunction f e1 || nameValueListContainsCall f nameValueList
                                                                                where nameValueListContainsCall f [] = False
                                                                                      nameValueListContainsCall f ((_,e):rest) = exprContainsCallToFunction f e || nameValueListContainsCall f (rest)

exprContainsCallToFunction f (FunctionCallExpressionList e Nothing) = exprContainsCallToFunction f e
exprContainsCallToFunction f (FunctionCallExpressionList e (Just (ExpressionList exs))) = exprContainsCallToFunction f e || foldr ((||) . exprContainsCallToFunction f) False exs
exprContainsCallToFunction f (Literal (PrimaryExpressionIdentifier ff)) = f == ff
exprContainsCallToFunction _ _ = False


warningsSpecification :: Specification -> [String]
warningsSpecification spec =
  concat (map warningsContractSpecification cspecs)
  where
    cspecs = contractSpecifications spec

warningsContractSpecification :: ContractSpecification -> [String]
warningsContractSpecification cspec
  | null warnings = []
  | otherwise = ("\n\nWarnings in definition of specification of contract <"++display (contractName cspec)++">:"):warnings
  where
    warnings =
      
      [ "\n   You seem to be lacking logic to enable the instrumented smart contract. Ensure you have a call to LARVA_EnableContract in the initialisation block or in a function the declarations block.\n\n"
      | not (monitorDeclarationsHasCustomEnablingLogic cspec ||monitorInitialisationHasCustomEnablingLogic cspec)
      ]
