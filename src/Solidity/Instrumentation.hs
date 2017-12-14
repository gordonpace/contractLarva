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

  getAllDefinedContractNames, getAllDefinedFunctionNames, getAllDefinedModifierNames,
  getFunctionParameters,

  addContractPart, addContractParts,
  addGlobalVariableDeclarationToContract, addModifierDefinitionToContract,
  addFunctionDefinitionToContract, addTypeDefinitionToContract,

  renameContract,
  renameFunctionInContract, renameFunctionsInContract,
  renameModifierInContract, renameModifiersInContract,

  addTopModifierToFunctionInContract,
  addTopModifierToFunctionsInContract,
  addTopModifierToAllButTheseFunctionInContract,

  functionIsPublicInContract,
  defineAndUseSetterFunctionForVariableInContract,

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

functionIsPublicInContract :: ContractName -> FunctionName -> SolidityCode -> Bool
functionIsPublicInContract cn fn code =
  let Just (ContractPartFunctionDefinition _ _ ts _ _) = getFunctionFromContract cn fn code
  in  FunctionDefinitionTagPrivate `notElem` ts

variableIsDefinedInContract :: ContractName -> VariableName -> SolidityCode -> Bool
variableIsDefinedInContract cn vn code =
  let Just c = getContract cn code
  in  not $ null [ visibility vd |  ContractPartStateVariableDeclaration vd <- contractParts c, variableName vd == vn ]

variableIsPublicInContract :: ContractName -> VariableName -> SolidityCode -> Bool
variableIsPublicInContract cn vn code =
  let Just c = getContract cn code
  in  "private" `notElem` head [ visibility vd |  ContractPartStateVariableDeclaration vd <- contractParts c, variableName vd == vn ]

getVariableTypeInContract :: ContractName -> VariableName -> SolidityCode -> TypeName
getVariableTypeInContract cn vn code =
  let Just c = getContract cn code
  in  head [ typename vd |  ContractPartStateVariableDeclaration vd <- contractParts c, variableName vd == vn ]

defineAndUseSetterFunctionForVariableInContract :: ContractName -> VariableName -> (FunctionName, FunctionName) -> Instrumentation
defineAndUseSetterFunctionForVariableInContract cn vn (fnPreValue, fnPostValue) code 
  | variableIsDefinedInContract cn vn code = 
      addFunctionDefinitionToContract cn (parseDeclaration setterFunctionPreValue) $
      addFunctionDefinitionToContract cn (parseDeclaration setterFunctionPostValue) $
      useSetterForVariableInContract cn vn (fnPreValue, fnPostValue) $
      makeVariablePrivateInContract cn vn $
      addGlobalVariableDeclarationToContract cn (parseDeclaration previousVariableValue)
      code 
  | otherwise = code
  where
    variableType = getVariableTypeInContract cn vn code
    visibilityOfVariable = if variableIsPublicInContract cn vn code then "public" else "private"

    f = display fnPreValue
    f' = display fnPostValue
    v = display vn
    t = display variableType
    previousVariableValue = t++" private LARVA_previous_"++v++";"
    setterFunctionPostValue =
      "function "++f'++"("++t++" _"++v++") "++visibilityOfVariable++" returns ("++t++") { "++
        "LARVA_previous_"++v++" = "++v++"; "++v++" = _"++v++"; return "++v++"; }"
    setterFunctionPreValue =
      "function "++f++"("++t++" _"++v++") "++visibilityOfVariable++" returns ("++t++") { "++
        "LARVA_previous_"++v++" = "++v++"; "++v++" = _"++v++"; return LARVA_previous_"++v++"; }"

-- PARSING STUFF LOCALLY

parseDeclaration :: String -> ContractPart
parseDeclaration = fromRight undefined . parse parser ""

-- CONTRACT MODIFIERS
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

addTopModifierToFunctionsInContract :: ContractName -> [FunctionName] -> (ModifierName, ExpressionList) -> Instrumentation
addTopModifierToFunctionsInContract cn fs (mn, es) =
  addModifierToFunctionsWithinContract cn (`elem` fs) (mn, es)

addTopModifierToAllButTheseFunctionInContract :: ContractName -> [FunctionName] -> (ModifierName, ExpressionList) -> Instrumentation
addTopModifierToAllButTheseFunctionInContract cn fs (mn, es) = 
  addModifierToFunctionsWithinContract cn (\f -> f `notElem` fs) (mn, es)

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
  renameFunctionsWithinContract :: Identifier -> (Identifier -> Identifier) -> a -> a
  addModifierToFunctionsWithinContract :: Identifier -> (Identifier -> Bool) -> (Identifier, ExpressionList) -> a -> a

  getModifierFromContract :: Identifier -> Identifier -> a -> Maybe ContractPart
  renameModifiersWithinContract :: Identifier -> (Identifier -> Identifier) -> a -> a

  makeVariablePrivateInContract :: Identifier -> Identifier -> a -> a
  useSetterForVariableInContract :: Identifier -> Identifier -> (Identifier, Identifier) -> a -> a
  -- (given contract, variable name, setter function names (return previous value, return after value))

  contractsDefined _ = []
  functionsDefined _ = []
  modifiersDefined _ = []

  getContract _ _ = Nothing
  renameContract _ = id
  updateContract _ _ = id

  getFunctionFromContract _ _ _ = Nothing
  renameFunctionsWithinContract _ _ = id
  addModifierToFunctionsWithinContract _ _ _ = id

  getModifierFromContract _ _ _ = Nothing
  renameModifiersWithinContract _ _ = id

  makeVariablePrivateInContract _ _ = id
  useSetterForVariableInContract _ _ _ = id


instance SolidityNode SolidityCode where
  getContract cn (SolidityCode u) = getContract cn u
  renameContract cc' (SolidityCode u) = SolidityCode $ renameContract cc' u
  updateContract cn update (SolidityCode u) = SolidityCode (updateContract cn update u)

  getFunctionFromContract cn fn (SolidityCode u) = getFunctionFromContract cn fn u
  renameFunctionsWithinContract cn renaming (SolidityCode u) = SolidityCode $ renameFunctionsWithinContract cn renaming u
  addModifierToFunctionsWithinContract cn fn m (SolidityCode u) = SolidityCode $ addModifierToFunctionsWithinContract cn fn m u

  getModifierFromContract cn mn (SolidityCode u) = getModifierFromContract cn mn u
  renameModifiersWithinContract cn renaming (SolidityCode u) = SolidityCode $ renameModifiersWithinContract cn renaming u

  contractsDefined (SolidityCode u) = contractsDefined u
  functionsDefined (SolidityCode u) = functionsDefined u
  modifiersDefined (SolidityCode u) = modifiersDefined u

  makeVariablePrivateInContract cn vn c@(SolidityCode u)
    | variableIsDefinedInContract cn vn c = SolidityCode $ makeVariablePrivateInContract cn vn u
  makeVariablePrivateInContract _ _ c = c 
  useSetterForVariableInContract cn vn fns (SolidityCode u) = SolidityCode $ useSetterForVariableInContract cn vn fns u


instance SolidityNode SourceUnit where
  getContract cn (SourceUnit us) = msum $ map (getContract cn) us
  renameContract cc' (SourceUnit us) = SourceUnit $ map (renameContract cc') us
  updateContract cn update (SourceUnit us) = SourceUnit (map (updateContract cn update) us)

  getFunctionFromContract cn fn (SourceUnit us) = msum $ map (getFunctionFromContract cn fn) us
  renameFunctionsWithinContract cn renaming (SourceUnit us) = SourceUnit $ map (renameFunctionsWithinContract cn renaming) us
  addModifierToFunctionsWithinContract cn fn m (SourceUnit us) = SourceUnit $ map (addModifierToFunctionsWithinContract cn fn m) us

  getModifierFromContract cn fn (SourceUnit us) = msum $ map (getModifierFromContract cn fn) us
  renameModifiersWithinContract cn renaming (SourceUnit us) = SourceUnit $ map (renameModifiersWithinContract cn renaming) us

  contractsDefined (SourceUnit us) = concatMap contractsDefined us
  functionsDefined (SourceUnit us) = concatMap functionsDefined us
  modifiersDefined (SourceUnit us) = concatMap modifiersDefined us

  makeVariablePrivateInContract cn vn (SourceUnit us) = SourceUnit $ map (makeVariablePrivateInContract cn vn) us
  useSetterForVariableInContract cn vn fns (SourceUnit us) = SourceUnit $ map (useSetterForVariableInContract cn vn fns) us


instance SolidityNode SourceUnit1 where
  getContract cn (SourceUnit1_ContractDefinition c) = getContract cn c
  getContract _ _ = Nothing

  renameContract cc' (SourceUnit1_ContractDefinition c) = SourceUnit1_ContractDefinition $ renameContract cc' c
  renameContract _ su = su

  updateContract cn update (SourceUnit1_ContractDefinition c) = SourceUnit1_ContractDefinition (updateContract cn update c)
  updateContract _ _ u = u

  getFunctionFromContract cn fn (SourceUnit1_ContractDefinition c) = getFunctionFromContract cn fn c
  getFunctionFromContract _ _ _ = Nothing

  renameFunctionsWithinContract cn renaming (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ renameFunctionsWithinContract cn renaming c
  renameFunctionsWithinContract _ _ c = c

  addModifierToFunctionsWithinContract cn fn m (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ addModifierToFunctionsWithinContract cn fn m c
  addModifierToFunctionsWithinContract _ _ _ u = u

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

  useSetterForVariableInContract cn vn fns (SourceUnit1_ContractDefinition c) =
    SourceUnit1_ContractDefinition $ useSetterForVariableInContract cn vn fns c
  useSetterForVariableInContract _ _ _ su = su

instance SolidityNode ContractDefinition where
  getContract cn c | definitionType c == "contract" && definitionName c == cn = Just c
  getContract _ _ = Nothing

  renameContract (cn,cn') c
    | definitionType c == "contract" && definitionName c == cn = c { definitionName = cn' }
  renameContract _ c = c

  updateContract contractName updateFunction c
    | definitionType c == "contract" && definitionName c == contractName = updateFunction c
  updateContract _ _ c = c

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

  useSetterForVariableInContract cn vn fns c
    | definitionType c == "contract" && definitionName c == cn
    = c { contractParts = map (useSetterForVariableInContract cn vn fns) (contractParts c) }
  useSetterForVariableInContract _ _ _ c = c


instance SolidityNode ContractPart where
  getContract _ _ = Nothing

  renameContract _ c = c

  getFunctionFromContract _ fn f@(ContractPartFunctionDefinition (Just fn') _ _ _ _) | fn' == fn = Just f
  getFunctionFromContract _ _ _ = Nothing

  renameFunctionsWithinContract _ renaming (ContractPartFunctionDefinition (Just fn) ps ts rs b) =
    ContractPartFunctionDefinition (Just (renaming fn)) ps ts rs b
  renameFunctionsWithinContract _ _ cp = cp

  addModifierToFunctionsWithinContract _ fn (mn, es) (ContractPartFunctionDefinition (Just f) pl ts pl' b)
    | fn f = ContractPartFunctionDefinition (Just f) pl (t:ts) pl' b
    where
      t = FunctionDefinitionTagModifierInvocation
        ModifierInvocation {
          modifierInvocationIdentifier = mn,
          modifierInvocationParameters = if null (unExpressionList es) then Nothing else Just es
        }
  addModifierToFunctionsWithinContract _ _ _ p = p

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

  useSetterForVariableInContract cn vn fns (ContractPartModifierDefinition mn pl b) =
    ContractPartModifierDefinition mn pl $ useSetterForVariableInContract cn vn fns b
  useSetterForVariableInContract cn vn fns (ContractPartFunctionDefinition mfn pl ts rt (Just b)) =
    ContractPartFunctionDefinition mfn pl ts rt $ Just $ useSetterForVariableInContract cn vn fns b
  useSetterForVariableInContract _ _ _ cp = cp


instance SolidityNode Block where
  useSetterForVariableInContract cn vn fns (Block ss) =
    Block $ map (useSetterForVariableInContract cn vn fns) ss


instance SolidityNode Statement where
  useSetterForVariableInContract cn vn fns (IfStatement e s1 ms2) =
    IfStatement
      (useSetterForVariableInContract cn vn fns e)
        (useSetterForVariableInContract cn vn fns s1)
          (useSetterForVariableInContract cn vn fns <$> ms2)
  useSetterForVariableInContract cn vn fns (WhileStatement e s) =
    WhileStatement (useSetterForVariableInContract cn vn fns e) (useSetterForVariableInContract cn vn fns s)
  useSetterForVariableInContract cn vn fns (DoWhileStatement s e) =
    DoWhileStatement (useSetterForVariableInContract cn vn fns s) (useSetterForVariableInContract cn vn fns e)
  useSetterForVariableInContract cn vn fns (BlockStatement b) =
    BlockStatement $ useSetterForVariableInContract cn vn fns b
  useSetterForVariableInContract cn vn fns (Return (Just e)) =
    Return $ Just $ useSetterForVariableInContract cn vn fns e
  useSetterForVariableInContract cn vn fns (ForStatement (ms, me1, me2) s1) =
    ForStatement
      (useSetterForVariableInContract cn vn fns <$> ms,
        useSetterForVariableInContract cn vn fns <$> me1,
          useSetterForVariableInContract cn vn fns <$> me2)
            (useSetterForVariableInContract cn vn fns s1)
  useSetterForVariableInContract cn vn fns (SimpleStatementExpression e) =
    SimpleStatementExpression $ useSetterForVariableInContract cn vn fns e
  useSetterForVariableInContract cn vn fns (SimpleStatementVariableDeclaration vd e) =
    SimpleStatementVariableDeclaration vd (useSetterForVariableInContract cn vn fns <$> e)
  useSetterForVariableInContract cn vn fns (SimpleStatementVariableList il e) =
    SimpleStatementVariableList il (useSetterForVariableInContract cn vn fns <$> e)
  useSetterForVariableInContract _ _ _ s = s


instance SolidityNode Expression where
  -- here
  useSetterForVariableInContract _ vn (fnPreValue,fnPostValue) (Unary op e)
    | op `elem` assignmentOperators && assignmentToVariable =
      FunctionCallExpressionList
        (Literal (PrimaryExpressionIdentifier fn))
          (Just (ExpressionList [e']))
    where
      assignmentOperators = ["++","--","()--","()++"]
      assignmentToVariable = e == Literal (PrimaryExpressionIdentifier vn)

      fn = if head op == '(' then fnPreValue else fnPostValue -- _++/_-- uses the pre-value, otherwise use post-value
      op' = if '+' `elem` op then "+" else "-" -- change ++ into addition, -- into subtraction
      e' = Binary op' (Unary "()" e) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing)))

  useSetterForVariableInContract cn vn fns (Unary op e) = Unary op $ useSetterForVariableInContract cn vn fns e
  useSetterForVariableInContract cn vn fns@(_,fn) (Binary op e1 e2)
    | op `elem` assignmentOperators && assignmentToVariable =
      FunctionCallExpressionList
        (Literal (PrimaryExpressionIdentifier fn))
          (Just (ExpressionList [e2'']))
    where
      assignmentOperators = ["=", "|=", "^=", "&=", "<<=", ">>=", "+=", "-=", "*=", "/=", "%="]
      assignmentToVariable = e1 == Literal (PrimaryExpressionIdentifier vn)
      e2' = useSetterForVariableInContract cn vn fns e2
      e2'' = if op == "=" then e2' else Binary (init op) (Unary "()" e1) (Unary "()" e2')
  useSetterForVariableInContract cn vn fns (Binary op e1 e2) =
    Binary op (useSetterForVariableInContract cn vn fns e1) (useSetterForVariableInContract cn vn fns e2)
  useSetterForVariableInContract cn vn fns (Ternary op e1 e2 e3) =
    Ternary op
      (useSetterForVariableInContract cn vn fns e1)
        (useSetterForVariableInContract cn vn fns e2)
          (useSetterForVariableInContract cn vn fns e3)
  useSetterForVariableInContract cn vn fns (FunctionCallNameValueList e ps) =
    FunctionCallNameValueList (useSetterForVariableInContract cn vn fns e) ps
  useSetterForVariableInContract cn vn fns (FunctionCallExpressionList e ps) =
    FunctionCallExpressionList
      (useSetterForVariableInContract cn vn fns e)
        (useSetterForVariableInContract cn vn fns <$> ps)
  useSetterForVariableInContract cn vn fns (MemberAccess e i) =
    MemberAccess (useSetterForVariableInContract cn vn fns e) i
  useSetterForVariableInContract _ _ _ e = e


instance SolidityNode ExpressionList where
  useSetterForVariableInContract cn vn fns el =
    ExpressionList $ map (useSetterForVariableInContract cn vn fns) (unExpressionList el)


