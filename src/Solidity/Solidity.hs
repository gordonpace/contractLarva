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

{-# LANGUAGE FlexibleContexts #-}

-- Bug in Sublime Text syntax highlighting for Haskel ("()"...

module Solidity.Solidity (
  SolidityCode (..),
    SourceUnit (..),
      SourceUnit1 (..),
    PragmaDirective (..),
      VersionComparator (..), Version (..),
    ImportDirective (..),
      ImportDirective1(..), Import (..),
    ContractDefinition (..),
      ContractPart (..),
      StateVariableDeclaration (..),
      InheritanceSpecifier (..),
      ModifierInvocation (..),
      FunctionDefinitionTag (..),
        VariableDeclaration (..),
        Statement (..),
        TupleExpression (..), ExpressionList (..), Expression (..), PrimaryExpression (..), NameValueList (..),
        NumberLiteral (..), NumberUnit (..), HexLiteral (..), StringLiteral (..), BooleanLiteral (..),
        InlineAssemblyBlock (..), AssemblyItem (..), FunctionalAssemblyExpression (..),
        Block (..),

  IdentifierList (..), Identifier (..),
  IndexedParameterList (..), IndexedParameter (..),
  UntypedParameterList (..), ParameterList (..), Parameter (..),
  TypeNameList (..), TypeName (..), UserDefinedTypeName (..), ElementaryTypeName (..),
  StateMutability (..), StorageLocation (..),

  FunctionName, VariableName, ContractName, ModifierName,

  untypeParameterList, typeParameterList, addMemoryLocationToParametersList
) where

import Data.Maybe

type FunctionName = Identifier
type VariableName = Identifier
type ContractName = Identifier
type ModifierName = Identifier

-------------------------------------------------------------------------------
newtype SolidityCode = SolidityCode SourceUnit deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- SourceUnit = (PragmaDirective | ImportDirective | ContractDefinition)*

newtype SourceUnit = SourceUnit [SourceUnit1] deriving (Show, Eq, Ord)

data SourceUnit1
  = SourceUnit1_PragmaDirective PragmaDirective
  | SourceUnit1_ImportDirective ImportDirective
  | SourceUnit1_ContractDefinition ContractDefinition
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- VersionComparator = '^' | '>' | '<' | '<=' | '>='

data VersionComparator = Less | More | Equal | LessOrEqual | MoreOrEqual deriving (Show, Eq, Ord)

-- Version = VersionComparator ([0-9]+\.)+

data Version = Version VersionComparator [Int] deriving (Show, Eq, Ord)

-- PragmaDirective = 'pragma' ('solidity' | 'experimental' ) 
--                       ( (VersionComparator ' ' Version) ('||' (VersionComparator ' ' Version))*
--                        | (VersionComparator ' ' Version) (' ' (VersionComparator ' ' Version))*) ';'

data PragmaDirective = SolidityPragmaConjunction [Version] 
                     | SolidityPragmaDisjunction [Version] 
                     | ExperimentalPragma String deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- ImportDirective = 'import' StringLiteral ('as' Identifier)? ';'
--         | 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
--         | 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'

data ImportDirective =
  ImportDirective {
    imports :: [ImportDirective1],
    from :: StringLiteral
  } deriving (Show, Eq, Ord)

data ImportDirective1 =
  ImportDirective1 {
    name :: Import,
    as   :: Maybe Identifier
  } deriving (Show, Eq, Ord)
data Import = ImportAll | ImportId Identifier deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- ContractDefinition = ( 'contract' | 'library' | 'interface' ) Identifier
--                      ( 'is' InheritanceSpecifier (',' InheritanceSpecifier )* )?
--                      '{' ContractPart* '}'

data ContractDefinition =
  ContractDefinition {
    definitionType :: String,
    definitionName :: Identifier,
    isClause :: [InheritanceSpecifier],
    contractParts :: [ContractPart]
  } deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- ContractPart
--    = 'using' Identifier 'for' ('*' | TypeName) ';'
--    | 'struct' Identifier '{' ( VariableDeclaration ';' (VariableDeclaration ';')* )? '}'
--    | 'modifier' Identifier ParameterList? Block
--    | 'constructor' ParameterList ( FunctionDefinitionTag )* ( ';' | Block )
--    | 'function' Identifier? ParameterList  ( FunctionDefinitionTag )* ( 'returns' ParameterList )? ( ';' | Block )
--    | 'enum' Identifier '{' EnumValue? (',' EnumValue)* '}'
--    | 'event' Identifier IndexedParameterList 'anonymous'? ';'
--    | StateVariableDeclaration

data ContractPart
  = ContractPartUsingForDeclaration Identifier (Maybe TypeName)
  | ContractPartStructDefinition Identifier [VariableDeclaration]
  | ContractPartModifierDefinition Identifier (Maybe ParameterList) Block
  | ContractPartConstructorDefinition ParameterList [FunctionDefinitionTag] (Maybe Block)
  | ContractPartFunctionDefinition (Maybe Identifier) ParameterList [FunctionDefinitionTag] (Maybe ParameterList) (Maybe Block)
  | ContractPartEnumDefinition Identifier [EnumValue]
  | ContractPartEventDefinition Identifier IndexedParameterList Bool
  | ContractPartStateVariableDeclaration StateVariableDeclaration
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- StateVariableDeclaration = TypeName ( 'public' | 'internal' | 'private' | 'constant' )? Identifier ('=' Expression)? ';'

data StateVariableDeclaration = StateVariableDeclaration {
    typename :: TypeName,
    visibility :: [String],
    variableName :: Identifier,
    initialValue :: Maybe Expression
  } deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- InheritanceSpecifier = UserDefinedTypeName ( '(' Expression ( ',' Expression )* ')' )?

data InheritanceSpecifier =
  InheritanceSpecifier { userDefinedTypeName :: UserDefinedTypeName, inheritanceParameters :: [Expression] } deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- ModifierInvocation = Identifier ( '(' ExpressionList? ')' )?

data ModifierInvocation =
  ModifierInvocation {
    modifierInvocationIdentifier :: Identifier,
    modifierInvocationParameters :: Maybe ExpressionList
  } deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- FunctionDefinitionTag = ModifierInvocation | StateMutability | 'public' | 'internal' | 'private'

data FunctionDefinitionTag
  = FunctionDefinitionTagModifierInvocation ModifierInvocation
  | FunctionDefinitionTagStateMutability StateMutability
  | FunctionDefinitionTagPublic
  | FunctionDefinitionTagPrivate
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- EnumValue = Identifier

type EnumValue = Identifier

-------------------------------------------------------------------------------
-- IndexedParameterList =
--  '(' ( TypeName 'indexed'? Identifier? (',' TypeName 'indexed'? Identifier?)* )? ')'

newtype IndexedParameterList = IndexedParameterList [IndexedParameter] deriving (Eq, Ord, Show)

data IndexedParameter = IndexedParameter {
    indexedParameterType :: TypeName,
    indexedParameterIndexed :: Bool,
    indexedParameterIdentifier :: Maybe Identifier
  } deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- UntypedParameterList = '(' ( Identifier (',' Identifier)* )? ')'
-- Added for DEAs

newtype UntypedParameterList = UntypedParameterList { fromUntypedParameterList :: [Identifier] } deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- ParameterList = '(' ( TypeName StorageLocation? Identifier? (',' TypeName StorageLocation? Identifier?)* )? ')'

newtype ParameterList = ParameterList [Parameter] deriving (Eq, Ord, Show)

data Parameter = Parameter {
    parameterType :: TypeName,
    parameterStorageLocation :: Maybe StorageLocation,
    parameterIdentifier :: Maybe Identifier
  } deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- TypeNameList = '(' ( TypeName (',' TypeName )* )? ')'

newtype TypeNameList = TypeNameList [TypeName] deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- VariableDeclaration = TypeName StorageLocation? Identifier
data VariableDeclaration = VariableDeclaration {
    variableDeclarationType :: TypeName,
    variableDeclarationStorageLocation :: Maybe StorageLocation,
    variableDeclarationName :: Identifier
  } deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- TypeName
--          = 'mapping' '(' ElementaryTypeName '=>' TypeName ')'
--          | ElementaryTypeName
--          | 'function' TypeNameList ( StateMutability )* ( 'returns' TypeNameList )?
--          | UserDefinedTypeName

--          | TypeName '[' Expression? ']'

data TypeName
  = TypeNameMapping ElementaryTypeName TypeName
  | TypeNameFunctionTypeName TypeNameList [StateMutability] (Maybe TypeNameList)
  | TypeNameElementaryTypeName ElementaryTypeName
  | TypeNameUserDefinedTypeName UserDefinedTypeName
  | TypeNameArrayTypeName TypeName (Maybe Expression)
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- UserDefinedTypeName = Identifier ( '.' Identifier )*

data UserDefinedTypeName = UserDefinedTypeName [Identifier] deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- StorageLocation = 'memory' | 'storage' | 'calldata'

data StorageLocation = Memory | Storage | CallData deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- StateMutability = 'internal' | 'external' | 'pure' | 'constant' | 'view' | 'payable'

data StateMutability = Pure | Constant | View | Payable | Internal | External deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- IdentifierList = '(' ( Identifier? ',' )* Identifier? ')'

newtype IdentifierList = IdentifierList [Identifier] deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Block = '{' Statement* '}'

newtype Block = Block [Statement] deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Statement = IfStatement | WhileStatement | ForStatement | Block | InlineAssemblyStatement |
--             ( DoWhileStatement | PlaceholderStatement | Continue | Break | Return |
--               Throw | SimpleStatement ) ';'
--
-- IfStatement = 'if' '(' Expression ')' Statement ( 'else' Statement )?
-- WhileStatement = 'while' '(' Expression ')' Statement
-- InlineAssemblyStatement = 'assembly' StringLiteral? InlineAssemblyBlock
-- ForStatement = 'for' '(' (SimpleStatement)? ';' (Expression)? ';' (Expression)? ')' Statement
--
-- DoWhileStatement = 'do' Statement 'while' '(' Expression ')'
-- PlaceholderStatement = '_'
-- Continue = 'continue'
-- Break = 'break'
-- Return = 'return' Expression?
-- Throw = 'throw'
-- EmitStatement = 'emit' Expression
-- SimpleStatement =
--    Expression | ('var' IdentifierList ( '=' Expression ) | VariableDeclaration ( '=' Expression )?
data Statement
  = IfStatement Expression Statement (Maybe Statement)
  | WhileStatement Expression Statement
  | InlineAssemblyStatement (Maybe StringLiteral) InlineAssemblyBlock
  | ForStatement (Maybe Statement, Maybe Expression, Maybe Expression) Statement
  | BlockStatement Block

  | DoWhileStatement Statement Expression
  | PlaceholderStatement
  | Continue
  | Break
  | Return (Maybe Expression)
  | Throw
  | EmitStatement Expression

  | SimpleStatementExpression Expression
  | SimpleStatementVariableList IdentifierList (Maybe Expression)
 -- | SimpleStatementVariableDeclaration VariableDeclaration (Maybe Expression)
  | SimpleStatementVariableDeclarationList [Maybe VariableDeclaration] [Expression]
  | SimpleStatementVariableAssignmentList [Maybe Identifier] [Expression]
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
--  Precedence by order (see github.com/ethereum/solidity/pull/732)
-- Expression
--    = Expression ('++' | '--')
--    | Expression '[' Expression? ']'                                         -- index access
--    | '(' Expression ')'
--    | ('!' | '~' | 'delete' | '++' | '--' | '+' | '-') Expression
--    | Expression '**' Expression
--    | Expression ('*' | '/' | '%') Expression
--    | Expression ('+' | '-') Expression
--    | Expression ('<<' | '>>') Expression
--    | Expression '&' Expression
--    | Expression '^' Expression
--    | Expression '|' Expression
--    | Expression ('<' | '>' | '<=' | '>=') Expression
--    | Expression ('==' | '!=') Expression
--    | Expression '&&' Expression
--    | Expression '||' Expression
--    | Expression '?' Expression ':' Expression
--    | Expression ('=' | '|=' | '^=' | '&=' | '<<=' | '>>=' | '+=' | '-=' | '*=' | '/=' | '%=') Expression

--   | PrimaryExpression
--   | Expression '(' ('{' NameValueList? '}' | ExpressionList? ) ')'         -- function call
--   | Expression '.' Identifier                                              -- member access
--   | 'new' Typename

data Expression
  = Unary String Expression
  | Binary String Expression Expression
  | Ternary String Expression Expression Expression
  | FunctionCallNameValueList Expression (Maybe NameValueList)
  | FunctionCallExpressionList Expression (Maybe ExpressionList)
  | MemberAccess Expression Identifier
  | Literal PrimaryExpression
  | New TypeName
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- PrimaryExpression = BooleanLiteral
--                   | NumberLiteral
--                   | HexLiteral
--                   | StringLiteral
--                   | TupleExpression
--                   | Identifier
--                   | ElementaryTypeNameExpression
data PrimaryExpression
  = PrimaryExpressionBooleanLiteral BooleanLiteral
  | PrimaryExpressionNumberLiteral NumberLiteral
  | PrimaryExpressionHexLiteral HexLiteral
  | PrimaryExpressionStringLiteral StringLiteral
  | PrimaryExpressionTupleExpression TupleExpression
  | PrimaryExpressionIdentifier Identifier
  | PrimaryExpressionElementaryTypeNameExpression ElementaryTypeNameExpression
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- ExpressionList = Expression ( ',' Expression )*

newtype ExpressionList = ExpressionList { unExpressionList :: [Expression] } deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- NameValueList = Identifier ':' Expression ( ',' Identifier ':' Expression )*

newtype NameValueList = NameValueList [(Identifier, Expression)] deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- BooleanLiteral = 'true' | 'false'

newtype BooleanLiteral = BooleanLiteral String deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- NumberLiteral = ( HexNumber | DecimalNumber ) (' ' NumberUnit)?

-- HexNumber = '0x' [0-9a-fA-F]+
-- DecimalNumber = [0-9]+

data NumberLiteral
  = NumberLiteralHex String (Maybe NumberUnit)
  | NumberLiteralDec String (Maybe NumberUnit)
 deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- NumberUnit = 'wei' | 'szabo' | 'finney' | 'ether'
--           | 'seconds' | 'minutes' | 'hours' | 'days' | 'weeks' | 'years'

data NumberUnit
  = Wei | Szabo | Finney | Ether | Seconds | Minutes | Hours | Days | Weeks | Years
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')

newtype HexLiteral = HexLiteral String deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'

newtype StringLiteral = StringLiteral String deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Identifier = [a-zA-Z_$] [a-zA-Z_$0-9]*

newtype Identifier = Identifier { unIdentifier :: String } deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------------
-- TupleExpression = '(' ( Expression ( ',' Expression )*  )? ')'
--                 | '[' ( Expression ( ',' Expression )*  )? ']'

data TupleExpression
  = RoundBrackets [Expression]
  | SquareBrackets [Expression]
  deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------------
-- ElementaryTypeNameExpression = ElementaryTypeName

type ElementaryTypeNameExpression = ElementaryTypeName

-- -------------------------------------------------------------------------------
-- ElementaryTypeName = 'address' | 'bool' | 'string' | 'var'
--                    | Int | Uint | Byte | Fixed | Ufixed
--
-- Int = 'int' | 'int8' | 'int16' | 'int24' | 'int32' | 'int40' | 'int48' | 'int56' | 'int64' | 'int72' | 'int80' | 'int88' | 'int96' | 'int104' | 'int112' | 'int120' | 'int128' | 'int136' | 'int144' | 'int152' | 'int160' | 'int168' | 'int176' | 'int184' | 'int192' | 'int200' | 'int208' | 'int216' | 'int224' | 'int232' | 'int240' | 'int248' | 'int256'
-- Uint = 'uint' | 'uint8' | 'uint16' | 'uint24' | 'uint32' | 'uint40' | 'uint48' | 'uint56' | 'uint64' | 'uint72' | 'uint80' | 'uint88' | 'uint96' | 'uint104' | 'uint112' | 'uint120' | 'uint128' | 'uint136' | 'uint144' | 'uint152' | 'uint160' | 'uint168' | 'uint176' | 'uint184' | 'uint192' | 'uint200' | 'uint208' | 'uint216' | 'uint224' | 'uint232' | 'uint240' | 'uint248' | 'uint256'
-- Byte = 'byte' | 'bytes' | 'bytes1' | 'bytes2' | 'bytes3' | 'bytes4' | 'bytes5' | 'bytes6' | 'bytes7' | 'bytes8' | 'bytes9' | 'bytes10' | 'bytes11' | 'bytes12' | 'bytes13' | 'bytes14' | 'bytes15' | 'bytes16' | 'bytes17' | 'bytes18' | 'bytes19' | 'bytes20' | 'bytes21' | 'bytes22' | 'bytes23' | 'bytes24' | 'bytes25' | 'bytes26' | 'bytes27' | 'bytes28' | 'bytes29' | 'bytes30' | 'bytes31' | 'bytes32'
-- Fixed = 'fixed' | ( 'fixed' DecimalNumber 'x' DecimalNumber )
-- Ufixed = 'ufixed' | ( 'ufixed' DecimalNumber 'x' DecimalNumber )

data ElementaryTypeName
  = AddressPayableType | AddressType | BoolType | StringType | VarType
  | IntType (Maybe Integer) | UintType (Maybe Integer) | BytesType (Maybe Integer)
  | ByteType | FixedType (Maybe (Integer, Integer)) | UfixedType (Maybe (Integer, Integer))
  deriving (Eq, Ord, Show)

-- -------------------------------------------------------------------------------
-- InlineAssemblyBlock = '{' AssemblyItem* '}'

newtype InlineAssemblyBlock = InlineAssemblyBlock [AssemblyItem] deriving (Eq, Ord, Show)

-- -------------------------------------------------------------------------------
-- AssemblyItem = Identifier | FunctionalAssemblyExpression | InlineAssemblyBlock | AssemblyLocalBinding | AssemblyAssignment | AssemblyLabel | NumberLiteral | StringLiteral | HexLiteral
-- AssemblyLabel = Identifier ':'
-- AssemblyLocalBinding = 'let' Identifier ':=' FunctionalAssemblyExpression
-- AssemblyAssignment = ( Identifier ':=' FunctionalAssemblyExpression ) | ( '=:' Identifier )

data AssemblyItem
  = AssemblyItemFunctionalAssemblyExpression FunctionalAssemblyExpression
  | AssemblyItemInlineAssemblyBlock InlineAssemblyBlock
  | AssemblyItemAssemblyLocalBinding Identifier FunctionalAssemblyExpression
  | AssemblyItemAssemblyAssignment Identifier FunctionalAssemblyExpression
  | AssemblyItemNumberLiteral NumberLiteral
  | AssemblyItemStringLiteral StringLiteral
  | AssemblyItemHexLiteral HexLiteral
  | AssemblyItemAssemblyLabel Identifier
  | AssemblyItemIdentifier Identifier
  deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------------
-- FunctionalAssemblyExpression = Identifier '(' AssemblyItem? ( ',' AssemblyItem )* ')'

data FunctionalAssemblyExpression = FunctionalAssemblyExpression Identifier [AssemblyItem] deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------------


typeParameterList :: UntypedParameterList -> ParameterList -> ParameterList
typeParameterList (UntypedParameterList ups) (ParameterList tps) =
  ParameterList $ zipWith
    (\parameter_name typed_parameter -> typed_parameter { parameterIdentifier = Just parameter_name })
    ups
    (tps ++
      repeat (
        Parameter {
          parameterType = TypeNameUserDefinedTypeName (UserDefinedTypeName [Identifier "undefined"]),
          parameterStorageLocation = Nothing,
          parameterIdentifier = Just (Identifier "undefined")
        }
      )
    )

untypeParameterList :: ParameterList -> UntypedParameterList
untypeParameterList (ParameterList ps) = UntypedParameterList $ map (fromJust . parameterIdentifier) ps

-- -------------------------------------------------------------------------------

addMemoryLocationToListOfParameters :: [Parameter] -> [Parameter]
addMemoryLocationToListOfParameters [] = []
addMemoryLocationToListOfParameters ((Parameter (TypeNameArrayTypeName t e) Nothing id):rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                                                               in ((Parameter (TypeNameArrayTypeName t e) (Just Memory) id): newRest)
addMemoryLocationToListOfParameters ((Parameter (TypeNameElementaryTypeName StringType) Nothing id):rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                                                                            in ((Parameter (TypeNameElementaryTypeName StringType) (Just Memory) id): newRest)
addMemoryLocationToListOfParameters ((Parameter (TypeNameElementaryTypeName (BytesType no)) Nothing id):rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                                                                                in ((Parameter (TypeNameElementaryTypeName (BytesType no)) (Just Memory) id): newRest)
addMemoryLocationToListOfParameters (x:rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                in (x:newRest)


addMemoryLocationToParametersList :: ParameterList -> ParameterList
addMemoryLocationToParametersList (ParameterList ps) = ParameterList (addMemoryLocationToListOfParameters ps)