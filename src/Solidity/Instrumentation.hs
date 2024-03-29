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

module Solidity.Instrumentation (
  Instrumentation, 

  oldStyleConstructorUsed, newStyleConstructorUsed, useNewStyleConstructor, constructorIsDefinedInContract, 
  conditionalInstrumentation, addTopModifierToContractConstructor,
  getAllDefinedContractNames, getAllDefinedFunctionNames, getAllDefinedModifierNames,
  getFunctionParameters,

  addContractPart, addContractParts,
  addGlobalVariableDeclarationToContract, addModifierDefinitionToContract,
  addFunctionDefinitionToContract, addTypeDefinitionToContract,

  renameContract, renameConstructorInContract,
  renameFunctionInContract, renameFunctionsInContract,
  renameModifierInContract, renameModifiersInContract,

  addTopModifierToFunctionInContract,
  addTopModifierToFunctionsInContract,
  addTopModifierToAllButTheseFunctionInContract,

  functionIsPublicInContract, functionIsDefinedInContract,
  defineAndUseSetterFunctionForVariableInContract, defineAndUseSetterFunctionForTransferInContract, defineAndUseSetterFunctionForSelfDestructInContract,

  variableIsDefinedInContract, variableIsPublicInContract, getVariableTypeInContract,

  information
) where

import Debug.Trace
import Solidity.Solidity
import Solidity.Parsing

import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad

type Filename = String

type Instrumentation = SolidityCode -> SolidityCode

-- CAPTURE INFORMATION

getAllDefinedContractNames :: SolidityCode -> [ContractName]
getAllDefinedContractNames = contractsDefined

getAllDefinedFunctionNames :: SolidityCode -> [(ContractName, [FunctionName])]
getAllDefinedFunctionNames s =
  [ (cn, functionsDefined $ fromJust $ getContract cn s)
  | cn <- contractsDefined s
  ]

getAllDefinedModifierNames :: SolidityCode -> [(ContractName, [ModifierName])]
getAllDefinedModifierNames s =
  [ (cn, modifiersDefined $ fromJust $ getContract cn s)
  | cn <- contractsDefined s
  ]

functionIsDefinedInContract :: ContractName -> FunctionName -> SolidityCode -> Bool
functionIsDefinedInContract cn fn code = case getFunctionFromContract cn fn code of
                                                Just _ -> True
                                                _ -> False

functionIsPublicInContract :: ContractName -> FunctionName -> SolidityCode -> Bool
functionIsPublicInContract cn fn code =
  let Just (ContractPartFunctionDefinition _ _ ts _ _) = getFunctionFromContract cn fn code
  in  FunctionDefinitionTagPrivate `notElem` ts

variableIsDefinedInContract :: ContractName -> VariableName -> SolidityCode -> Bool
variableIsDefinedInContract cn vn code =
  case getContract cn code of
    Nothing -> False
    Just c -> not $ null [ visibility vd |  ContractPartStateVariableDeclaration vd <- contractParts c, variableName vd == vn ]

variableIsPublicInContract :: ContractName -> VariableName -> SolidityCode -> Bool
variableIsPublicInContract cn vn code =
  let Just c = getContract cn code
  in not $ null [ "public" |  ContractPartStateVariableDeclaration vd <- contractParts c, variableName vd == vn, "public" `elem` (visibility vd)]

getVariableTypeInContract :: ContractName -> VariableName -> SolidityCode -> TypeName
getVariableTypeInContract cn vn code =
  let Just c = getContract cn code
  in  head [ typename vd |  ContractPartStateVariableDeclaration vd <- contractParts c, variableName vd == vn ]

getStructVariableNamesInContract :: ContractName -> TypeName -> SolidityCode -> [Identifier]
getStructVariableNamesInContract cn (TypeNameUserDefinedTypeName (UserDefinedTypeName (name:ids))) code =
  let Just c = getContract cn code
  in [ variableDeclarationName vd |  ContractPartStructDefinition id vds <- contractParts c, name == id, vd <- vds]
getStructVariableNamesInContract _ _ _ = []

getStructVariableNamesAndTypesInContract :: ContractName -> TypeName -> SolidityCode -> [(Identifier, TypeName)]
getStructVariableNamesAndTypesInContract cn (TypeNameUserDefinedTypeName (UserDefinedTypeName (name:ids))) code =
  let Just c = getContract cn code
  in [ (variableDeclarationName vd, variableDeclarationType vd) |  ContractPartStructDefinition id vds <- contractParts c, name == id, vd <- vds]
getStructVariableNamesAndTypesInContract _ _ _ = []

isUserDefinedTypeName :: TypeName -> Bool
isUserDefinedTypeName (TypeNameUserDefinedTypeName _) = True
isUserDefinedTypeName x = False

defineAndUseSetterFunctionForTransferInContract :: ContractName -> Instrumentation
defineAndUseSetterFunctionForTransferInContract cn code = 
      addFunctionDefinitionToContract cn (parseDeclaration transferFunction) $
      useSetterForVariableInContract cn (Identifier "unused") (Identifier "LARVA_transfer", Identifier "unused") code $
      code
  where
    transferFunction = "function LARVA_transfer(address payable _to, uint amount) internal {_to.transfer(amount);}"

defineAndUseSetterFunctionForSelfDestructInContract :: ContractName -> Instrumentation
defineAndUseSetterFunctionForSelfDestructInContract cn code = 
      addFunctionDefinitionToContract cn (parseDeclaration selfdestructFunction) $
      useSetterForVariableInContract cn (Identifier "unused") (Identifier "LARVA_selfdestruct", Identifier "unused") code $
      code
  where
    selfdestructFunction = "function LARVA_selfdestruct(address payable _to) internal {selfdestruct(_to);}"

defineAndUseSetterFunctionForVariableInContract :: ContractName -> VariableName -> (FunctionName, FunctionName) -> Instrumentation
defineAndUseSetterFunctionForVariableInContract cn vn (fnPreValue, fnPostValue) code 
  | variableIsDefinedInContract cn vn code = 
      addFunctionDefinitionsToContract cn (map parseDeclaration setterFunctionPreValues) $
      addFunctionDefinitionsToContract cn (map parseDeclaration setterFunctionPostValues) $
      useSetterForVariableInContract cn vn (fnPreValue, fnPostValue) code $
      makeVariablePrivateInContract cn vn $
      addGlobalVariableDeclarationToContract cn (parseDeclaration previousVariableValue) $
      code
  | otherwise = code
  where
    variableType = getVariableTypeInContract cn vn code
    visibilityOfVariable = if variableIsPublicInContract cn vn code then "public" else "private"
    
    structType = isUserDefinedTypeName
    
    indexedType (TypeNameArrayTypeName typeName expr) = True
    indexedType (TypeNameMapping elementaryTypeName typeName) = True
    indexedType x = False

    keyParam (TypeNameArrayTypeName typeName expr) = TypeNameElementaryTypeName (IntType Nothing)
    keyParam (TypeNameMapping elementaryTypeName typeName) = TypeNameElementaryTypeName elementaryTypeName
    keyParam x = x

    valueType (TypeNameArrayTypeName typeName expr) = typeName
    valueType (TypeNameMapping elementaryTypeName typeName) = typeName
    valueType x = x

    f = display fnPreValue
    f' = display fnPostValue
    v = display vn
    key = display $ keyParam variableType
    value = display $ valueType variableType

    previousVariableValue = value++" private LARVA_previous_"++v++";"
    setterFunctionPostValues = 
      if indexedType variableType
        then ["function "++f'++"("++key++" _index, "++value++" _"++v++"_value) "++"internal"++" returns ("++value++") { "++
              "LARVA_previous_"++v++"_value = "++v++"; "++v++"[_index] = _"++v++"_value; return "++v++"; }"]
        else (if structType variableType
                then structFieldCheckingCode v False
                else [])
              ++ ["function "++f'++"("++value++" _"++v++") "++"internal"++" returns ("++value++") { "++
                    "LARVA_previous_"++v++" = "++v++"; "++v++" = _"++v++"; return "++v++"; }"]
    setterFunctionPreValues = 
      if indexedType variableType
        then ["function "++f++"("++key++" _index, "++value++" _"++v++"_value) "++"internal"++" returns ("++value++") { "++
                "LARVA_previous_"++v++"_value = "++v++"; "++v++"[_index] = _"++v++"_value; return LARVA_previous_"++v++"; }"]
        else (if structType variableType
                then structFieldCheckingCode v True
                else [])
              ++ ["function "++f++"("++value++" _"++v++") "++"internal"++" returns ("++value++") { "++
                      "LARVA_previous_"++v++" = "++v++"; "++v++" = _"++v++"; return LARVA_previous_"++v++"; }"]

    structFieldCheckingCode1 :: String -> Bool -> [(Identifier,TypeName)] -> [String]
    structFieldCheckingCode1 name _ [] = []
    structFieldCheckingCode1 name pre ((id,t):ids) = ["function "++f++"_"++unIdentifier id++"("++display t++" _value) "++"internal"++" returns ("++display t++") { "++
                      "LARVA_previous_"++v++" = "++v++"; "++v++"."++unIdentifier id++"= _value;" ++ 
                      (if pre
                        then " return LARVA_previous_"++v++"; }"
                        else " return "++v++"; }")]
                    ++ structFieldCheckingCode1 name pre ids

    structFieldCheckingCode name pre = structFieldCheckingCode1 name pre(getStructVariableNamesAndTypesInContract cn variableType code)

-- DEALING WITH CONSTRUCTORS

--Old denotes old-style of constructors using name of smart contract, allowable before version 0.5
--New denotes new-style of constructors using <constructors> keyword, allowed after version 0.4.23 and obliged for versions >= 0.5
--Both denotes when both styles are acceptable (>=0.4.23 and <0.5)

--                Old
--            <-----------|            Both
--                          |---------------------------|      New
--                                                        |------------>
--            ------------------------------------------------------
--                        | |                           | |
--                   0.4.22 0.4.23                 0.4.26 0.5

data ConstructorStyle = OldStyle | NewStyle | Both deriving (Eq)

--Functions to detect whether a version number is bigger or less than anothe
biggerThan :: [Int] -> [Int] -> Bool
biggerThan (v1:vs1) (v2:vs2)  
                | v1 > v2 = True
                | v1 < v2 = False
                | otherwise = biggerThan vs1 vs2

biggerThan (v:vs) [] = True
biggerThan [] _ = False

lessThan :: [Int] -> [Int] -> Bool
lessThan (v1:vs1) (v2:vs2) 
                | v1 < v2 = True
                | v1 > v2 = False
                | otherwise = lessThan vs1 vs2
                
lessThan [] (v:vs) = True
lessThan _ [] = False

--Computing the style of constructors allowed by the specified version
constructorStylesPossible :: Version -> [ConstructorStyle]

constructorStylesPossible (Version Equal no) 
                    | not (no `biggerThan` [0,4,23] || no == [0,4,23]) = [OldStyle]
                    | no `biggerThan` [0,5] || no == [0,5] = [NewStyle]
                    | otherwise = [Both]
  
constructorStylesPossible (Version Less no) 
                    | no `lessThan` [0,4,23] || no == [0,4,23] = [OldStyle]
                    | no `lessThan` [0,5] || no == [0,5] = [OldStyle, Both]
                    | otherwise = [NewStyle, OldStyle, Both]
  
constructorStylesPossible (Version More no) 
                    | no `biggerThan` [0,4,26] || no == [0,4,26] = [NewStyle]
                    | no `biggerThan` [0,4,22] || no == [0,4,22] = [NewStyle, Both]
                    | otherwise = [OldStyle, NewStyle, Both]
  
constructorStylesPossible (Version LessOrEqual no)
                    | no `lessThan` [0,4,22] = [OldStyle]
                    | no `lessThan` [0,4,26] = [OldStyle, Both]
                    | otherwise = [NewStyle, OldStyle, Both]
  
constructorStylesPossible (Version MoreOrEqual no) 
                    | no `biggerThan` [0,5] = [NewStyle]
                    | no `biggerThan` [0,4,23] = [NewStyle, Both]
                    | otherwise = [OldStyle, NewStyle, Both]
  

--Check whether a new style or an old style constructor should be used for the contracts in the specified smart contracts
-- for both a conjunction and disjunction of version ranges the intersection of the styles of each version is computed
-- a new style constructor is then used if the computed styles include NewStyle or if they only contain Both
-- an old style constructor is used otherwise, i.e. if NewStyle is not in the computed styles and OldStyle is
-- 
-- Note how if the version range specified allows for non-compatible compilers (i.e. OldStyle, NewStyle <- styles ) 
--  then we default to using the new compiler and leave it up to the user to manually correct the contract during compilation
-- Note also how the intersection of version styles is used for both conjunction and disjunction of versions to remain conservative
useNewStyleConstructor :: SolidityCode -> Bool
useNewStyleConstructor (SolidityCode (SourceUnit (SourceUnit1_PragmaDirective (SolidityPragmaConjunction versions):rest)))
    = let styles = foldr (intersect . constructorStylesPossible) [NewStyle, OldStyle, Both] versions
        in styles == [Both] || NewStyle `elem` styles


useNewStyleConstructor (SolidityCode (SourceUnit (SourceUnit1_PragmaDirective (SolidityPragmaDisjunction versions):rest)))
    = let styles = foldr (intersect . constructorStylesPossible) [] versions
        in styles == [Both] || NewStyle `elem` styles

useNewStyleConstructor (SolidityCode (SourceUnit (_:rest))) = useNewStyleConstructor (SolidityCode (SourceUnit rest))
useNewStyleConstructor _ = True

--Checking if a constructor is defined in the smart contract, and in which style
constructorIsDefinedInContract :: ContractName -> SolidityCode -> Bool
constructorIsDefinedInContract cn code = oldStyleConstructorUsed cn code || newStyleConstructorUsed cn code

oldStyleConstructorUsed :: ContractName -> SolidityCode -> Bool
oldStyleConstructorUsed cn = functionIsDefinedInContract cn cn

newStyleConstructorUsed :: ContractName -> SolidityCode -> Bool
newStyleConstructorUsed cn code = case getContract cn code of
                                    Just c -> not $ null [ "" |  ContractPartConstructorDefinition _ _ _ <- contractParts c]
                                    Nothing -> False

-- PARSING STUFF LOCALLY

parseDeclaration :: String -> ContractPart
parseDeclaration = fromRight undefined . parse parser ""

-- CONTRACT MODIFIERS
conditionalInstrumentation :: (SolidityCode -> Bool) -> Instrumentation -> Instrumentation
conditionalInstrumentation pred instr x = if pred x
                                                then instr x
                                                else x


addContractParts :: ContractName -> [ContractPart] -> Instrumentation
addContractParts contractName contractParts =
  updateContract contractName (insertContractParts contractParts)

addContractPart :: ContractName -> ContractPart -> Instrumentation
addContractPart contractName contractPart =
  addContractParts contractName [contractPart]

addGlobalVariableDeclarationToContract, addModifierDefinitionToContract,
  addFunctionDefinitionToContract, addTypeDefinitionToContract ::
    ContractName -> ContractPart -> Instrumentation
addGlobalVariableDeclarationToContract = addContractPart
addModifierDefinitionToContract = addContractPart
addFunctionDefinitionToContract = addContractPart
addTypeDefinitionToContract = addContractPart

addFunctionDefinitionsToContract ::
    ContractName -> [ContractPart] -> Instrumentation
addFunctionDefinitionsToContract cn [] code = code
addFunctionDefinitionsToContract cn (cp:cps) code = 
  (addFunctionDefinitionToContract cn cp)
  $ (addFunctionDefinitionsToContract cn cps) 
  $ code

renameModifierInContract :: ContractName -> (ModifierName, ModifierName) -> Instrumentation
renameModifierInContract contractName (modifierName, modifierName') =
  renameModifiersInContract contractName (\modifierNameIn -> if modifierNameIn == modifierName then modifierName' else modifierNameIn)

renameModifiersInContract :: ContractName -> (ModifierName -> ModifierName) -> Instrumentation
renameModifiersInContract = renameModifiersWithinContract

renameFunctionInContract :: ContractName -> (FunctionName, FunctionName) -> Instrumentation
renameFunctionInContract contractName (functionName, functionName') =
  renameFunctionsInContract contractName (\functionNameIn -> if functionNameIn == functionName then functionName' else functionNameIn)

renameFunctionsInContract :: ContractName -> (FunctionName -> FunctionName) -> Instrumentation
renameFunctionsInContract = renameFunctionsWithinContract

addTopModifierToFunctionInContract :: ContractName -> FunctionName -> (ModifierName, ExpressionList) -> Instrumentation
addTopModifierToFunctionInContract cn f (mn, es) =
  addModifierToFunctionsWithinContract cn (f==) (mn, es)

addTopModifierToContractConstructor :: ContractName -> (ModifierName, ExpressionList) -> Instrumentation
addTopModifierToContractConstructor cn (mn, es) =
  addModifierToContractConstructor cn (mn, es)

addTopModifierToFunctionsInContract :: ContractName -> [FunctionName] -> (ModifierName, ExpressionList) -> Instrumentation
addTopModifierToFunctionsInContract cn fs (mn, es) =
  addModifierToFunctionsWithinContract cn (`elem` fs) (mn, es)

addTopModifierToAllButTheseFunctionInContract :: ContractName -> [FunctionName] -> (ModifierName, ExpressionList) -> Instrumentation
addTopModifierToAllButTheseFunctionInContract cn fs (mn, es) = 
  addModifierToFunctionsWithinContract cn (`notElem` fs) (mn, es)

getFunctionParameters :: ContractName -> FunctionName -> SolidityCode -> ParameterList
getFunctionParameters cn fn code = 
  case getFunctionFromContract cn fn code of
    Nothing -> ParameterList []
    Just (ContractPartFunctionDefinition _ ps _ _ _) -> ps 


-- TOP LEVEL FUNCTIONALITY

information :: Filename -> IO ()
information inputFilename =
  do
    code <- readFile inputFilename
    case parse (parser :: Parser SolidityCode) "" code of
      Left e  -> putStrLn ("ERROR while parsing:\n"++unlines (map ("   "++) $ lines $ show e))
      Right c ->
        putStrLn $
          unlines
            [ "File successfully parsed"
            , "Contracts defined: " ++ show (getAllDefinedContractNames c)
            , "Functions defined: " ++ show (getAllDefinedFunctionNames c)
            ]


-- Helper functions
insertContractParts :: [ContractPart] -> ContractDefinition -> ContractDefinition
insertContractParts newContractParts contractDefinition  =
  contractDefinition { contractParts = newContractParts ++ contractParts contractDefinition }

-- Processing the parse tree

class SolidityNode a where
  contractsDefined :: a -> [Identifier]
  functionsDefined :: a -> [Identifier]
  modifiersDefined :: a -> [Identifier]

  getContract :: Identifier -> a -> Maybe ContractDefinition
  renameContract :: (Identifier, Identifier) -> a -> a
  updateContract :: Identifier -> (ContractDefinition -> ContractDefinition) -> a -> a

  getFunctionFromContract :: Identifier -> Identifier -> a -> Maybe ContractPart
  renameConstructorInContract :: Identifier -> Identifier -> a -> a
  renameFunctionsWithinContract :: Identifier -> (Identifier -> Identifier) -> a -> a
  addModifierToFunctionsWithinContract :: Identifier -> (Identifier -> Bool) -> (Identifier, ExpressionList) -> a -> a
  addModifierToContractConstructor :: Identifier -> (Identifier, ExpressionList) -> a -> a

  getModifierFromContract :: Identifier -> Identifier -> a -> Maybe ContractPart
  renameModifiersWithinContract :: Identifier -> (Identifier -> Identifier) -> a -> a

  makeVariablePrivateInContract :: Identifier -> Identifier -> a -> a
  useSetterForVariableInContract :: Identifier -> Identifier -> (Identifier, Identifier) -> SolidityCode -> a -> a
  -- (given contract, variable name, setter function names, original code, (return previous value, return after value))

  contractsDefined _ = []
  functionsDefined _ = []
  modifiersDefined _ = []

  getContract _ _ = Nothing
  renameContract _ = id
  updateContract _ _ = id

  getFunctionFromContract _ _ _ = Nothing
  renameConstructorInContract _ _ = id
  renameFunctionsWithinContract _ _ = id
  addModifierToFunctionsWithinContract _ _ _ = id
  addModifierToContractConstructor _ _ = id

  getModifierFromContract _ _ _ = Nothing
  renameModifiersWithinContract _ _ = id

  makeVariablePrivateInContract _ _ = id
  useSetterForVariableInContract _ _ _ _ = id


instance SolidityNode SolidityCode where
  getContract cn (SolidityCode u) = getContract cn u
  renameContract cc' (SolidityCode u) = SolidityCode $ renameContract cc' u
  updateContract cn update (SolidityCode u) = SolidityCode (updateContract cn update u)
  
  getFunctionFromContract cn fn (SolidityCode u) = getFunctionFromContract cn fn u
  renameConstructorInContract cn newName (SolidityCode u) = SolidityCode $ renameConstructorInContract cn newName u
  renameFunctionsWithinContract cn renaming (SolidityCode u) = SolidityCode $ renameFunctionsWithinContract cn renaming u
  addModifierToFunctionsWithinContract cn fn m (SolidityCode u) = SolidityCode $ addModifierToFunctionsWithinContract cn fn m u
  addModifierToContractConstructor cn m (SolidityCode u) = SolidityCode $ addModifierToContractConstructor cn m u

  getModifierFromContract cn mn (SolidityCode u) = getModifierFromContract cn mn u
  renameModifiersWithinContract cn renaming (SolidityCode u) = SolidityCode $ renameModifiersWithinContract cn renaming u

  contractsDefined (SolidityCode u) = contractsDefined u
  functionsDefined (SolidityCode u) = functionsDefined u
  modifiersDefined (SolidityCode u) = modifiersDefined u

  makeVariablePrivateInContract cn vn c@(SolidityCode u)
    | variableIsDefinedInContract cn vn c = SolidityCode $ makeVariablePrivateInContract cn vn u
  makeVariablePrivateInContract _ _ c = c 
  useSetterForVariableInContract cn vn fns code (SolidityCode u) = SolidityCode $ useSetterForVariableInContract cn vn fns code u


instance SolidityNode SourceUnit where
  getContract cn (SourceUnit us) = msum $ map (getContract cn) us
  renameContract cc' (SourceUnit us) = SourceUnit $ map (renameContract cc') us
  updateContract cn update (SourceUnit us) = SourceUnit (map (updateContract cn update) us)

  getFunctionFromContract cn fn (SourceUnit us) = msum $ map (getFunctionFromContract cn fn) us
  renameConstructorInContract cn newName (SourceUnit us) = SourceUnit $ map (renameConstructorInContract cn newName) us
  renameFunctionsWithinContract cn renaming (SourceUnit us) = SourceUnit $ map (renameFunctionsWithinContract cn renaming) us
  addModifierToFunctionsWithinContract cn fn m (SourceUnit us) = SourceUnit $ map (addModifierToFunctionsWithinContract cn fn m) us
  addModifierToContractConstructor cn m (SourceUnit us) = SourceUnit $ map (addModifierToContractConstructor cn m) us

  
  getModifierFromContract cn fn (SourceUnit us) = msum $ map (getModifierFromContract cn fn) us
  renameModifiersWithinContract cn renaming (SourceUnit us) = SourceUnit $ map (renameModifiersWithinContract cn renaming) us

  contractsDefined (SourceUnit us) = concatMap contractsDefined us
  functionsDefined (SourceUnit us) = concatMap functionsDefined us
  modifiersDefined (SourceUnit us) = concatMap modifiersDefined us

  makeVariablePrivateInContract cn vn (SourceUnit us) = SourceUnit $ map (makeVariablePrivateInContract cn vn) us
  useSetterForVariableInContract cn vn fns code (SourceUnit us) = SourceUnit $ map (useSetterForVariableInContract cn vn fns code) us


instance SolidityNode SourceUnit1 where
  getContract cn (SourceUnit1_ContractDefinition c) = getContract cn c
  getContract _ _ = Nothing

  renameContract cc' (SourceUnit1_ContractDefinition c) = SourceUnit1_ContractDefinition $ renameContract cc' c
  renameContract _ su = su

  updateContract cn update (SourceUnit1_ContractDefinition c) = SourceUnit1_ContractDefinition (updateContract cn update c)
  updateContract _ _ u = u

  getFunctionFromContract cn fn (SourceUnit1_ContractDefinition c) = getFunctionFromContract cn fn c
  getFunctionFromContract _ _ _ = Nothing


  renameConstructorInContract cn newName (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ renameConstructorInContract cn newName c
  renameConstructorInContract _ _ c = c

  renameFunctionsWithinContract cn renaming (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ renameFunctionsWithinContract cn renaming c
  renameFunctionsWithinContract _ _ c = c

  addModifierToFunctionsWithinContract cn fn m (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ addModifierToFunctionsWithinContract cn fn m c
  addModifierToFunctionsWithinContract _ _ _ u = u

  addModifierToContractConstructor cn m (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ addModifierToContractConstructor cn m c
  addModifierToContractConstructor _ _ u = u
  
  getModifierFromContract cn fn (SourceUnit1_ContractDefinition c) = getModifierFromContract cn fn c
  getModifierFromContract _ _ _ = Nothing

  renameModifiersWithinContract cn renaming (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition (renameModifiersWithinContract cn renaming c)
  renameModifiersWithinContract _ _ c = c

  contractsDefined (SourceUnit1_ContractDefinition c) = contractsDefined c
  contractsDefined _ = []

  functionsDefined (SourceUnit1_ContractDefinition c) = functionsDefined c
  functionsDefined _ = []

  modifiersDefined (SourceUnit1_ContractDefinition c) = modifiersDefined c
  modifiersDefined _ = []

  makeVariablePrivateInContract cn vn (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ makeVariablePrivateInContract cn vn c
  makeVariablePrivateInContract _ _ su = su

  useSetterForVariableInContract cn vn fns code (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ useSetterForVariableInContract cn vn fns code c
  useSetterForVariableInContract _ _ _ _ su = su

instance SolidityNode ContractDefinition where
  getContract cn c | definitionType c == "contract" && definitionName c == cn = Just c
  getContract _ _ = Nothing

  renameContract (cn,cn') c
    | definitionType c == "contract" && definitionName c == cn = c { definitionName = cn' }
  renameContract _ c = c

  updateContract contractName updateFunction c
    | definitionType c == "contract" && definitionName c == contractName = updateFunction c
  updateContract _ _ c = c

  renameConstructorInContract cn newName c
    | definitionType c == "contract" && definitionName c == cn =
      c { contractParts = map (renameConstructorInContract cn newName) (contractParts c) }
  renameConstructorInContract _ _ c = c

  renameFunctionsWithinContract cn renaming c
    | definitionType c == "contract" && definitionName c == cn =
      c { contractParts = map (renameFunctionsWithinContract cn renaming) (contractParts c) }
  renameFunctionsWithinContract _ _ c = c

  getFunctionFromContract cn fn c | definitionType c == "contract" && definitionName c == cn =
    msum $ map (getFunctionFromContract cn fn) (contractParts c)
  getFunctionFromContract _ _ _ = Nothing

  addModifierToFunctionsWithinContract cn fn m c | definitionType c == "contract" && definitionName c == cn =
    c { contractParts = map (addModifierToFunctionsWithinContract cn fn m) (contractParts c) }
  addModifierToFunctionsWithinContract _ _ _ c = c

  addModifierToContractConstructor cn m c | definitionType c == "contract" && definitionName c == cn =
    c { contractParts = map (addModifierToContractConstructor cn m) (contractParts c) }
  addModifierToContractConstructor _ _ c = c

  renameModifiersWithinContract cn renaming c
    | definitionType c == "contract" && definitionName c == cn =
      c { contractParts = map (renameModifiersWithinContract cn renaming) (contractParts c) }
  renameModifiersWithinContract _ _ c = c

  getModifierFromContract cn fn c | definitionType c == "contract" && definitionName c == cn =
    msum $ map (getModifierFromContract cn fn) (contractParts c)
  getModifierFromContract _ _ _ = Nothing

  contractsDefined c | definitionType c == "contract" = [definitionName c]
  contractsDefined _ = []

  functionsDefined = concatMap functionsDefined . contractParts

  modifiersDefined = concatMap modifiersDefined . contractParts

  makeVariablePrivateInContract cn vn c
    | definitionType c == "contract" && definitionName c == cn
    = c { contractParts = map (makeVariablePrivateInContract cn vn) (contractParts c) }
  makeVariablePrivateInContract _ _ c = c

  useSetterForVariableInContract cn vn fns code c 
    | definitionType c == "contract" && definitionName c == cn
    = c { contractParts = map (useSetterForVariableInContract cn vn fns code) (contractParts c) }
  useSetterForVariableInContract _ _ _ _ c = c


instance SolidityNode ContractPart where
  getContract _ _ = Nothing

  renameContract _ c = c

  getFunctionFromContract _ fn f@(ContractPartFunctionDefinition (Just fn') _ _ _ _) | fn' == fn = Just f
  getFunctionFromContract _ _ _ = Nothing

  renameConstructorInContract _ newName (ContractPartConstructorDefinition ps ts b) =
    ContractPartFunctionDefinition (Just (newName)) ps ts Nothing b
  renameConstructorInContract _ _ cp = cp

  renameFunctionsWithinContract _ renaming (ContractPartFunctionDefinition (Just fn) ps ts rs b) =
    ContractPartFunctionDefinition (Just (renaming fn)) ps ts rs b
  renameFunctionsWithinContract _ _ cp = cp

  addModifierToFunctionsWithinContract _ fn (mn, es) (ContractPartFunctionDefinition (Just f) pl ts pl' b)
    | fn f = ContractPartFunctionDefinition (Just f) pl (t:ts) pl' b'
    where
      t = FunctionDefinitionTagModifierInvocation
        ModifierInvocation {
          modifierInvocationIdentifier = mn,
          modifierInvocationParameters = if null (unExpressionList es) then Nothing else Just es
        }
      b' = if isNothing b
              then Just $ Block [] 
              else b
  addModifierToFunctionsWithinContract _ _ _ p = p

  addModifierToContractConstructor _ (mn, es) (ContractPartConstructorDefinition pl ts b)
     = ContractPartConstructorDefinition pl (t:ts) b
    where
      t = FunctionDefinitionTagModifierInvocation
        ModifierInvocation {
          modifierInvocationIdentifier = mn,
          modifierInvocationParameters = if null (unExpressionList es) then Nothing else Just es
        }

  addModifierToContractConstructor cn (mn, es) (ContractPartFunctionDefinition (Just f) pl ts pl' b)
    | cn == f = ContractPartFunctionDefinition (Just f) pl (t:ts) pl' b'
    where
      t = FunctionDefinitionTagModifierInvocation
        ModifierInvocation {
          modifierInvocationIdentifier = mn,
          modifierInvocationParameters = if null (unExpressionList es) then Nothing else Just es
        }
      b' = if isNothing b
              then Just $ Block [] 
              else b
  addModifierToContractConstructor _ _ p = p

  getModifierFromContract _ mn m@(ContractPartModifierDefinition mn' _ _) | mn' == mn = Just m
  getModifierFromContract _ _ _ = Nothing

  renameModifiersWithinContract _ renaming (ContractPartModifierDefinition mn ps b) =
    ContractPartModifierDefinition (renaming mn) ps b
  renameModifiersWithinContract _ _ cp = cp

  contractsDefined _ = []

  functionsDefined (ContractPartFunctionDefinition (Just i) _ _ _ _) = [i]
  functionsDefined _ = []

  modifiersDefined (ContractPartModifierDefinition i _ _) = [i]
  modifiersDefined _ = []

  makeVariablePrivateInContract _ vn (ContractPartStateVariableDeclaration vd)
    | variableName vd == vn
    = ContractPartStateVariableDeclaration (vd { visibility = (visibility vd \\ ["public"]) `union` ["private"] })
  makeVariablePrivateInContract _ _ c = c

  useSetterForVariableInContract cn vn fns code (ContractPartModifierDefinition mn pl b) =
    ContractPartModifierDefinition mn pl $ useSetterForVariableInContract cn vn fns code b
  useSetterForVariableInContract cn vn fns code (ContractPartFunctionDefinition mfn pl ts rt (Just b)) =
    ContractPartFunctionDefinition mfn pl ts rt $ Just $ useSetterForVariableInContract cn vn fns code b
  useSetterForVariableInContract _ _ _ _ cp = cp


instance SolidityNode Block where
  useSetterForVariableInContract cn vn fns code (Block ss) =
    Block $ map (useSetterForVariableInContract cn vn fns code) ss


instance SolidityNode Statement where
  useSetterForVariableInContract cn vn fns code (IfStatement e s1 ms2) =
    IfStatement
      (useSetterForVariableInContract cn vn fns code e)
        (useSetterForVariableInContract cn vn fns code s1)
          (useSetterForVariableInContract cn vn fns code <$> ms2)
  useSetterForVariableInContract cn vn fns code (WhileStatement e s) =
    WhileStatement (useSetterForVariableInContract cn vn fns code e) (useSetterForVariableInContract cn vn fns code s)
  useSetterForVariableInContract cn vn fns code (DoWhileStatement s e) =
    DoWhileStatement (useSetterForVariableInContract cn vn fns code s) (useSetterForVariableInContract cn vn fns code e)
  useSetterForVariableInContract cn vn fns code (BlockStatement b) =
    BlockStatement $ useSetterForVariableInContract cn vn fns code b
  useSetterForVariableInContract cn vn fns code (Return (Just e)) =
    Return $ Just $ useSetterForVariableInContract cn vn fns code e
  useSetterForVariableInContract cn vn fns code (ForStatement (ms, me1, me2) s1) =
    ForStatement
      (useSetterForVariableInContract cn vn fns code <$> ms,
        useSetterForVariableInContract cn vn fns code <$> me1,
          useSetterForVariableInContract cn vn fns code <$> me2)
            (useSetterForVariableInContract cn vn fns code s1)
  useSetterForVariableInContract cn vn fns code (SimpleStatementExpression e) =
    SimpleStatementExpression $ useSetterForVariableInContract cn vn fns code e
  useSetterForVariableInContract cn vn fns code (SimpleStatementVariableDeclarationList vds es) =
    SimpleStatementVariableDeclarationList vds (useSetterForVariableInContract cn vn fns code <$> es)
  useSetterForVariableInContract cn vn fns code (SimpleStatementVariableAssignmentList ((Just id):[]) (e:[])) = 
    useSetterForVariableInContract cn vn fns code (SimpleStatementExpression $ Binary "=" (Literal (PrimaryExpressionIdentifier id)) e)

  useSetterForVariableInContract cn vn fns code (SimpleStatementVariableAssignmentList [] []) = (SimpleStatementVariableAssignmentList [] [])

  useSetterForVariableInContract cn vn fns code (SimpleStatementVariableAssignmentList (ids) (es))
    | length ids == 0 || length es == 0 = useSetterForVariableInContract cn vn fns code (SimpleStatementVariableAssignmentList (ids) (es))
    | length ids == length es = BlockStatement (Block (unfurl (SimpleStatementVariableAssignmentList (ids) (es))))
    | otherwise = if Just vn `elem` (ids)
                    then newStatement
                    else SimpleStatementVariableAssignmentList ids (useSetterForVariableInContract cn vn fns code <$> es)
        where 
          unfurl (SimpleStatementVariableAssignmentList (id:ids) (e:es)) = ((useSetterForVariableInContract cn vn fns code (SimpleStatementVariableAssignmentList [id] [e])) : unfurl (SimpleStatementVariableAssignmentList (ids) (es)))
          unfurl (SimpleStatementVariableAssignmentList [] []) = []
          rest = useSetterForVariableInContract cn vn fns code (SimpleStatementVariableAssignmentList (ids) (es))
          newVar = Identifier (unIdentifier vn ++ "_LARVA")
          newVarDecl = VariableDeclaration (getVariableTypeInContract cn vn code) Nothing newVar
          varDecStmt = SimpleStatementVariableDeclarationList [Just newVarDecl] []
          setLater = useSetterForVariableInContract cn vn fns code $ SimpleStatementExpression (Binary "=" (Literal (PrimaryExpressionIdentifier vn)) (Literal (PrimaryExpressionIdentifier newVar)))
          newIds = replaceWithInList vn newVar ids
          replaceWithInList _ _ [] = []
          replaceWithInList x y (Just z:zs) = if x == z then (Just y: (replaceWithInList x y zs)) else (Just z: (replaceWithInList x y zs))
          replaceWithInList x y (Nothing:zs) = (Nothing: (replaceWithInList x y zs))
          newStatement = BlockStatement (Block [varDecStmt, SimpleStatementVariableAssignmentList newIds (useSetterForVariableInContract cn vn fns code <$> es), setLater])
      
  useSetterForVariableInContract cn vn fns code (SimpleStatementVariableList il e) =
    SimpleStatementVariableList il (useSetterForVariableInContract cn vn fns code <$> e)
  useSetterForVariableInContract _ _ _ _ s = s


instance SolidityNode Expression where
  -- here
  useSetterForVariableInContract cn vn (fnPreValue,fnPostValue) code (Unary op e)
    | op `elem` assignmentOperators =
      if assignmentToVariable
        then FunctionCallExpressionList
              (Literal (PrimaryExpressionIdentifier fn))
                (Just (ExpressionList [e']))
        else if fst $ assignmentToMappingOrArray e
                 then FunctionCallExpressionList
                        (Literal (PrimaryExpressionIdentifier fn))
                          (Just (ExpressionList $ prependIfNothing (snd $ assignmentToMappingOrArray e) [e']))
                 else if fst $ assignmentToStruct e
                  then FunctionCallExpressionList
                        (Literal (PrimaryExpressionIdentifier (Identifier $ (unIdentifier fn)++"_"++(snd $ assignmentToStruct e))))
                          (Just (ExpressionList [e']))
                  else Unary op e
    where
      assignmentOperators = ["++","--","()--","()++"]
      assignmentToVariable = e == Literal (PrimaryExpressionIdentifier vn)
      assignmentToMappingOrArray (Binary "[]" (Literal (PrimaryExpressionIdentifier id)) index) = 
                                    if id == vn
                                        then (True, Just index)
                                        else (False, Nothing)
      assignmentToMappingOrArray _ = (False, Nothing)

      assignmentToStruct (MemberAccess (Literal (PrimaryExpressionIdentifier id)) i) = 
                                    if id == vn
                                        then (True, display i)
                                        else (False, "")
      assignmentToStruct _ = (False, "")


      fn = if head op == '(' then fnPreValue else fnPostValue -- _++/_-- uses the pre-value, otherwise use post-value
      op' = if '+' `elem` op then "+" else "-" -- change ++ into addition, -- into subtraction
      e' = Binary op' (Unary "()" e) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing)))
      prependIfNothing (Just expr) list = (expr:list)
      prependIfNothing (Nothing) list = (list)

  useSetterForVariableInContract cn vn fns code (Unary op e) = Unary op $ useSetterForVariableInContract cn vn fns code e
  useSetterForVariableInContract cn vn fns@(_,fn) code (Binary op e1 e2)
    | op `elem` assignmentOperators =
        if assignmentToVariable
          then FunctionCallExpressionList
                  (Literal (PrimaryExpressionIdentifier fn))
                    (Just (ExpressionList [e2'']))
          else if fst $ assignmentToMappingOrArray e1
                  then FunctionCallExpressionList
                          (Literal (PrimaryExpressionIdentifier fn))
                            (Just (ExpressionList $ prependIfNothing (snd $ assignmentToMappingOrArray e1) [e2'']))
                  else if fst $ assignmentToStruct e1
                          then FunctionCallExpressionList
                                (Literal (PrimaryExpressionIdentifier (Identifier $ (unIdentifier fn)++"_"++(snd $ assignmentToStruct e1))))
                                  (Just (ExpressionList [e2'']))
                          else Binary op e1 e2''
    where
      assignmentOperators = ["=", "|=", "^=", "&=", "<<=", ">>=", "+=", "-=", "*=", "/=", "%="]
      assignmentToVariable = e1 == Literal (PrimaryExpressionIdentifier vn)
      assignmentToMappingOrArray (Binary "[]" (Literal (PrimaryExpressionIdentifier id)) index) = 
                                    if id == vn
                                        then (True, Just index)
                                        else (False, Nothing)
      assignmentToMappingOrArray _ = (False, Nothing)

      assignmentToStruct (MemberAccess (Literal (PrimaryExpressionIdentifier id)) i) = 
                                    if id == vn
                                        then (True, display i)
                                        else (False, "")
      assignmentToStruct _ = (False, "")

      e2' = useSetterForVariableInContract cn vn fns code e2
      e2'' = if op == "=" then e2' else Binary (init op) (Unary "()" e1) (Unary "()" e2')
      prependIfNothing (Just expr) list = (expr:list)
      prependIfNothing (Nothing) list = (list)
  useSetterForVariableInContract cn vn fns code (Binary op e1 e2) =
    Binary op (useSetterForVariableInContract cn vn fns code e1) (useSetterForVariableInContract cn vn fns code e2)
  useSetterForVariableInContract cn vn fns code (Ternary op e1 e2 e3) =
    Ternary op
      (useSetterForVariableInContract cn vn fns code e1)
        (useSetterForVariableInContract cn vn fns code e2)
          (useSetterForVariableInContract cn vn fns code e3)
  useSetterForVariableInContract cn vn fns code (FunctionCallNameValueList e ps) =
    FunctionCallNameValueList (useSetterForVariableInContract cn vn fns code e) ps
  useSetterForVariableInContract cn _ ((Identifier "LARVA_transfer"),_) code ((FunctionCallExpressionList (MemberAccess target (Identifier "transfer")) (Just (ExpressionList expList)))) =
      FunctionCallExpressionList
            (Literal (PrimaryExpressionIdentifier (Identifier "LARVA_transfer")))
              (Just (ExpressionList (target:expList)))
  useSetterForVariableInContract cn _ ((Identifier "LARVA_selfdestruct"),_) code ((FunctionCallExpressionList (Literal (PrimaryExpressionIdentifier (Identifier "selfdestruct"))) (Just (ExpressionList expList)))) =
      FunctionCallExpressionList
            (Literal (PrimaryExpressionIdentifier (Identifier "LARVA_selfdestruct")))
              (Just (ExpressionList (expList)))
  useSetterForVariableInContract cn vn fns code (FunctionCallExpressionList e ps) =
    FunctionCallExpressionList
      (useSetterForVariableInContract cn vn fns code e)
        (useSetterForVariableInContract cn vn fns code <$> ps)    
  useSetterForVariableInContract cn vn fns code (MemberAccess e i) =
    MemberAccess (useSetterForVariableInContract cn vn fns code e) i
  useSetterForVariableInContract _ _ _ _ e = e

instance SolidityNode ExpressionList where
  useSetterForVariableInContract cn vn fns code el =
    ExpressionList $ map (useSetterForVariableInContract cn vn fns code) (unExpressionList el)


