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

module Solidity.Parsing (module Parseable) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Data.Char hiding (DecimalNumber)
import Data.List

import Parseable
import Solidity.Solidity

-- Some helper parsing functions
indent = unlines . map ("  "++) . lines

sep1, sep :: Char -> Parser a -> Parser [a]
sep1 c p = try ((:) <$> p <*> (whitespace *> char c *> whitespace *> sep1 c p)) <|> (return <$> p)
sep c p = try (sep1 c p) <|> return []

semicolonSep, semicolonSep1, commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep = sep ','
commaSep1 = sep1 ','
semicolonSep = sep ';'
semicolonSep1 = sep1 ';'

pair :: a -> b -> (a,b)
pair x y = (x,y)

comment :: Parser ()
comment =
  try (string "//" *> manyTill anyChar (eof <|> (newline *> return ())) *> return ()) <|>
  (string "/*" *> manyTill anyChar (try $ string "*/") *> return ())

whitespace, whitespace1 :: Parser ()
whitespace  = const () <$> many (try comment <|> (const () <$> space))
whitespace1 = (comment <|> spaces) >> whitespace

endOfWord :: Parser ()
endOfWord = eof <|> notFollowedBy (alphaNum <|> oneOf "$_")

list :: [Parser a] -> Parser [a]
list [] = return []
list (p:ps) = do { vp <- p; vps <- list ps; return (vp:vps) }

keyword :: String -> Parser String
keyword word = string word <* endOfWord

-- end of helper functions

-------------------------------------------------------------------------------
instance Parseable SolidityCode where
  display (SolidityCode c) = display c
  parser = whitespace *> (SolidityCode <$> parser) <* whitespace <* eof

-------------------------------------------------------------------------------
-- SourceUnit = (PragmaDirective | ImportDirective | ContractDefinition)*

instance Parseable SourceUnit where
  parser = SourceUnit <$> (
      many (parser <* whitespace)
    )
  display (SourceUnit us) = unlines $ map display us

instance Parseable SourceUnit1 where
  parser =
    try (SourceUnit1_PragmaDirective <$> parser) <|>
    try (SourceUnit1_ImportDirective <$> parser) <|>
    (SourceUnit1_ContractDefinition <$> parser)
  display (SourceUnit1_ContractDefinition contract_definition) = display contract_definition
  display (SourceUnit1_ImportDirective import_directive) = display import_directive
  display (SourceUnit1_PragmaDirective pragma_directive) = display pragma_directive

-------------------------------------------------------------------------------
-- data VersionComparator = Less | More | Equal | LessOrEqual | MoreOrEqual deriving (Show, Eq, Ord)

instance Parseable VersionComparator where
  parser = try $ choice[
                do 
                  string ">"
                  t <-  try(
                          do string "="
                             return MoreOrEqual
                        ) <|> return More
                  return t
              , do
                  string "<"
                  t <- try(
                          do string "="
                             return LessOrEqual
                        ) <|> return Less
                  return t
              , do
                  char '^'
                  return Equal
            ]
            <|> return Equal
  display More = ">"
  display Less = "<"
  display MoreOrEqual = ">="
  display LessOrEqual = "<="
  display Equal = "^"
  
--  data Version = Version VersionComparator [Int] deriving (Show, Eq, Ord)

instance Parseable Version where
  parser = do 
              c <- parser
              version <- (many digit) `sepBy` (char '.')
              return $ Version c (map (read :: [Char] -> Int) version)

  display (Version c version) = (display c) ++ (intercalate "." [show n | n <- version]) 

-- data PragmaDirective = SolidityPragmaConjunction [Version] 
--                      | SolidityPragmaDisjunction [Version] 
--                      | ExperimentalPragma String deriving (Show, Eq, Ord)

instance Parseable PragmaDirective where
  parser = (string "pragma" *> whitespace1) *> 
            (choice 
            [   do 
                  string "experimental"
                  whitespace1
                  t <-  manyTill anyChar (char ';')
                  return $ ExperimentalPragma t
              , do
                  string "solidity"
                  whitespace1
                  r <- try(
                              do 
                                vs <- (whitespace1 *> parser <* whitespace1) `sepBy` (string "||")
                                whitespace1 <* char ';'
                                return $ SolidityPragmaDisjunction (vs)
                          ) <|>
                            ( do 
                                vs <- (parser) `sepBy` (char ' ' *> whitespace)
                                whitespace1 *> char ';'
                                return $ SolidityPragmaConjunction vs
                            
                      )
                  return r
            ])
    
  display (SolidityPragmaConjunction versions) = "pragma solidity "++(intercalate " " (map display versions))++";"
  display (SolidityPragmaDisjunction versions) = "pragma solidity "++(intercalate "||" (map display versions))++";"
  display (ExperimentalPragma label) = "pragma experimental "++ (label)++";"


-------------------------------------------------------------------------------
-- ImportDirective = 'import' StringLiteral ('as' Identifier)? ';'
--         | 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
--         | 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'

instance Parseable ImportDirective where
  parser = keyword "import" *> whitespace1 *> choice [directive1, directive2, directive3] <* whitespace <* char ';'
    where
      parseIdentifierOrStar :: Parser Import
      parseIdentifierOrStar = (const ImportAll <$> char '*') <|> (ImportId <$> parser)

      parseMaybeAsIdentifier :: Parser (Maybe Identifier)
      parseMaybeAsIdentifier =
        (Just <$> (keyword "as" *> whitespace1 *> parser)) <|> return Nothing

      directive1 =
        do
          _from <- parser <* whitespace
          _as   <- parseMaybeAsIdentifier
          return ImportDirective {
              imports = [ ImportDirective1 { name = ImportAll, as = _as } ],
              from = _from
          }
      directive2 =
        do
          _name <- parseIdentifierOrStar <* whitespace1
          _as   <- parseMaybeAsIdentifier <* whitespace
          _from <- keyword "from" *> whitespace1 *> parser
          return ImportDirective {
              imports = [ ImportDirective1 { name = _name, as = _as } ],
              from = _from
          }
      directive3 =
        do
          char '{' *> whitespace
          _directives <- commaSep1 (
              do
                _name <- parser <* whitespace1
                _as <- parseMaybeAsIdentifier <* whitespace
                return ImportDirective1 { name = ImportId _name, as = _as }
            )
          char '}' *> whitespace
          _from <- keyword "from" *> whitespace1 *> parser
          return ImportDirective {
              imports = _directives,
              from = _from
          }
  display directive =
    (case map displayImport (imports directive) of
      [i] -> i
      is  -> "{ "++intercalate ", " is++" }"
    ) ++ " from "++display (from directive)
    where
      displayImport i =
        case name i of
          ImportAll           -> "*"++textAs
          ImportId identifier -> display identifier++textAs
          where
            textAs =
              case as i of
                Nothing         -> ""
                Just identifier -> " as "++display identifier


-------------------------------------------------------------------------------
-- ContractDefinition = ( 'contract' | 'library' | 'interface' ) Identifier
--                      ( 'is' InheritanceSpecifier (',' InheritanceSpecifier )* )?
--                      '{' ContractPart* '}'

instance Parseable ContractDefinition where
  parser =
    do
      _definitionType <- (keyword "contract" <|> keyword "library" <|> keyword "interface") <* whitespace
      _definitionName <- parser <* whitespace
      _isClause <- (try (keyword "is" *> whitespace *> commaSep1 parser) <|> return []) <* whitespace
      _contractParts <- char '{' *> whitespace *> many (parser <* whitespace) <* char '}'
      return ContractDefinition {
        definitionType = _definitionType,
        definitionName = _definitionName,
        isClause = _isClause,
        contractParts = _contractParts
      }
  display contractDefinition =
    definitionType contractDefinition ++ _display (definitionName contractDefinition) ++
    (if null isClauses then "" else " is " ++ intercalate ", " isClauses) ++
    " {\n"++indent (concatMap (\p -> display p++"\n") (contractParts contractDefinition))++"\n}"
    where
      isClauses = map display $ isClause contractDefinition


-------------------------------------------------------------------------------
-- ContractPart
--    = 'using' Identifier 'for' ('*' | TypeName) ';'
--    | 'struct' Identifier '{' ( VariableDeclaration ';' (VariableDeclaration ';')* )? '}'
--    | 'modifier' Identifier ParameterList? Block
--    | 'function' Identifier? ParameterList  ( FunctionDefinitionTag )* ( 'returns' ParameterList )? ( ';' | Block )
--    | 'enum' Identifier '{' EnumValue? (',' EnumValue)* '}'
--    | 'event' Identifier IndexedParameterList 'anonymous'? ';'
--    | StateVariableDeclaration

instance Parseable ContractPart where
  parser = try (choice
    [ do
        i <- (keyword "using" *> whitespace *> parser) <* whitespace <* keyword "for" <* whitespace
        tn <- ((const Nothing <$> char '*') <|> (Just <$> parser)) <* whitespace <* char ';'
        return (ContractPartUsingForDeclaration i tn)
    , do
        i <- keyword "struct" *> whitespace *> parser <* whitespace <* char '{' <* whitespace
        vs <- many (parser <* whitespace <* char ';' <* whitespace) <* char '}'
        return (ContractPartStructDefinition i vs)
    , do
        i <- keyword "modifier" *> whitespace *> parser <* whitespace
        pl <- ((Just <$> parser) <|> return Nothing) <* whitespace
        b <- parser
        return (ContractPartModifierDefinition i pl b)
    , do
        mi <- keyword "function" *> whitespace *> parser <* whitespace
        ps <- parser <* whitespace
        ts <- many (parser <* whitespace)
        mps' <- (try (Just <$> keyword "returns" *> whitespace *> parser) <|> return Nothing) <* whitespace
        b <- ((const Nothing <$> char ';') <|> (Just <$> parser))
        return (ContractPartFunctionDefinition mi ps ts mps' b)
    , do
        ps <- keyword "constructor" *> whitespace *> parser <* whitespace
        ts <- many (parser <* whitespace)
        b <- ((const Nothing <$> char ';') <|> (Just <$> parser))
        return (ContractPartConstructorDefinition ps ts b)
    , char 'e' *> (
        do
          i <- keyword "num" *> whitespace *> parser <* whitespace <* char '{' <* whitespace
          vs <- commaSep parser <* whitespace <* char '}'
          return (ContractPartEnumDefinition i vs)
        <|>
        do
          i <- keyword "vent" *> whitespace *> parser <* whitespace
          ipl <- parser <* whitespace
          a <- (const True <$> keyword "anonymous" <|> return False) <* whitespace <* char ';'
          return (ContractPartEventDefinition i ipl a)
      )
    ]) <|> (ContractPartStateVariableDeclaration <$> parser)

  display (ContractPartUsingForDeclaration v t) =
    "using "++display v++" for "++maybe "*" display t ++ ";"
  display (ContractPartEnumDefinition i vs) =
    "enum "++display i ++" {"++intercalate ", " (map display vs)++"}"
  display (ContractPartStructDefinition i vs) =
    "struct "++display i ++" {\n"++indent (intercalate ";\n" (map display vs) ++";")++"}"
  display (ContractPartModifierDefinition i pl b) =
    "modifier "++display i++maybe "" _display pl++_display b
  display (ContractPartFunctionDefinition mi pl ts mpl' mb) =
    "function" ++ maybe "" _display mi ++ _display pl ++
    concatMap _display ts ++ maybe "" ((" returns " ++) . display) mpl' ++ maybe ";" _display mb
  display (ContractPartConstructorDefinition pl ts mb) =
    "constructor" ++ _display pl ++
    concatMap _display ts ++ maybe ";" _display mb
  display (ContractPartEventDefinition i ipl a) =
    "event "++display i++_display ipl++(if a then " anonymous" else "") ++ ";"
  display (ContractPartStateVariableDeclaration v) = display v


-------------------------------------------------------------------------------
-- StateVariableDeclaration = TypeName ( 'public' | 'internal' | 'private' | 'constant' )? Identifier ('=' Expression)? ';'

instance Parseable StateVariableDeclaration where
  parser =
    do
      _typename <- parser <* whitespace
      _visibility <- many (choice
          [ try $ keyword "public"
          , try $ keyword "private"
          , try $ keyword "internal"
          , try $ keyword "constant"
          ] <* whitespace
        )
      _variableName <- parser <* whitespace
      _initialValue <- try (Just <$> (char '=' *> whitespace *> parser)) <|> return Nothing
      _ <- whitespace *> char ';'
      return StateVariableDeclaration {
        typename = _typename,
        visibility = _visibility,
        variableName = _variableName,
        initialValue = _initialValue
      }

  display v =
    display (typename v) ++
    (if null (visibility v) then "" else (" "++unwords (visibility v))) ++
    _display (variableName v) ++
    maybe "" (\i -> " = "++display i) (initialValue v)++
    ";"


-------------------------------------------------------------------------------
-- InheritanceSpecifier = UserDefinedTypeName ( '(' Expression ( ',' Expression )* ')' )?

instance Parseable InheritanceSpecifier where
  parser =
    do
      _userDefinedTypeName <- parser <* whitespace
      _inheritanceParameters <- try (char '(' *> whitespace *> commaSep1 parser <* whitespace <* char ')') <|> return []
      return InheritanceSpecifier {
        userDefinedTypeName = _userDefinedTypeName,
        inheritanceParameters = _inheritanceParameters
      }
  display spec =
    display (userDefinedTypeName spec) ++
    (if null (inheritanceParameters spec) then "" else "("++intercalate ", " (map display $ inheritanceParameters spec)++")")

-------------------------------------------------------------------------------
-- ModifierInvocation = Identifier ( '(' ExpressionList? ')' )?

instance Parseable ModifierInvocation where
  parser =
    do
      i <- parser <* whitespace
      _ <- if (display i `notElem` ["returns"]) then return () else mzero
      es <- try (char '(' *> whitespace *> parser <* whitespace <* char ')') <|> return Nothing
      return ModifierInvocation { modifierInvocationIdentifier = i, modifierInvocationParameters = es }
  display mi =
    display (modifierInvocationIdentifier mi) ++
    maybe "" (\s -> "("++display s++")") (modifierInvocationParameters mi)

-------------------------------------------------------------------------------
-- FunctionDefinitionTag = ModifierInvocation | StateMutability | 'public' | 'internal' | 'private'

instance Parseable FunctionDefinitionTag where
  display (FunctionDefinitionTagModifierInvocation m) = display m
  display (FunctionDefinitionTagStateMutability s) = display s
  display FunctionDefinitionTagPublic = "public"
  display FunctionDefinitionTagPrivate = "private"

  parser = choice
    [ try $ FunctionDefinitionTagModifierInvocation <$> parser
    , try $ FunctionDefinitionTagStateMutability <$> parser
    , try $ const FunctionDefinitionTagPublic <$> keyword "public"
    , const FunctionDefinitionTagPrivate <$> keyword "private"
    ]


-------------------------------------------------------------------------------
-- EnumValue = Identifier

type EnumValue = Identifier

-------------------------------------------------------------------------------
-- IndexedParameterList =
--  '(' ( TypeName 'indexed'? Identifier? (',' TypeName 'indexed'? Identifier?)* )? ')'

instance Parseable IndexedParameterList where
  display (IndexedParameterList ps) = "(" ++ intercalate ", " (map display ps) ++")"
  parser = IndexedParameterList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')

instance Parseable IndexedParameter where
  display ip =
    display (indexedParameterType ip) ++
    (if indexedParameterIndexed ip then " indexed" else "")++
    maybe "" _display (indexedParameterIdentifier ip)
  parser =
    do
      paramType <- parser <* whitespace
      indexed <- (try (return True <$> keyword "indexed") <|> return False) <* whitespace
      identifier <- parser
      return IndexedParameter {
        indexedParameterType = paramType,
        indexedParameterIndexed = indexed,
        indexedParameterIdentifier = identifier
      }

-------------------------------------------------------------------------------
-- UntypedParameterList = '(' ( Identifier (',' Identifier)* )? ')'

instance Parseable UntypedParameterList where
  display (UntypedParameterList ps) = "(" ++ intercalate ", " (map display ps) ++")"
  parser = UntypedParameterList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')

-------------------------------------------------------------------------------
-- ParameterList = '(' ( TypeName Identifier? (',' TypeName Identifier?)* )? ')'

instance Parseable ParameterList where
  display (ParameterList ps) = "(" ++ intercalate ", " (map display ps) ++")"
  parser = ParameterList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')

instance Parseable Parameter where
  display p =
    display (parameterType p) ++
    maybe "" _display (parameterStorageLocation p) ++
    maybe "" _display (parameterIdentifier p)
  parser =
    do
      paramType <- parser <* whitespace
      storageLocation <- (try (Just <$> parser) <|> return Nothing) <* whitespace
      identifier <- try (Just <$> parser) <|> return Nothing
      return Parameter {
        parameterType = paramType,
        parameterStorageLocation = storageLocation,
        parameterIdentifier = identifier
      }


-------------------------------------------------------------------------------
-- TypeNameList = '(' ( TypeName (',' TypeName )* )? ')'

instance Parseable TypeNameList where
  display (TypeNameList ts) = "(" ++ intercalate ", " (map display ts) ++")"
  parser = TypeNameList <$> (char '(' *> whitespace *> commaSep parser <* whitespace <* char ')')


-------------------------------------------------------------------------------
-- VariableDeclaration = TypeName StorageLocation? Identifier

instance Parseable VariableDeclaration where
  display v =
    display (variableDeclarationType v) ++
    maybe "" _display (variableDeclarationStorageLocation v) ++
    _display (variableDeclarationName v)
  parser =
    do
      t <- parser <* whitespace
      sl <- parser <* whitespace
      i <- parser
      return VariableDeclaration {
        variableDeclarationType = t,
        variableDeclarationStorageLocation = sl,
        variableDeclarationName = i
      }


-------------------------------------------------------------------------------
-- TypeName
--          = 'mapping' '(' ElementaryTypeName '=>' TypeName ')'
--          | ElementaryTypeName
--          | 'function' TypeNameList ( StateMutability )* ( 'returns' TypeNameList )?
--          | UserDefinedTypeName

--          | TypeName '[' Expression? ']'

instance Parseable TypeName where
  display (TypeNameMapping t1 t2) = "mapping ("++display t1++" => "++display t2++")"
  display (TypeNameElementaryTypeName t) = display t
  display (TypeNameUserDefinedTypeName t) = display t
  display (TypeNameArrayTypeName t me) = display t ++"["++maybe "" display me++"]"
  display (TypeNameFunctionTypeName tl ms mtl') =
    "function " ++ display tl ++ (if null ms then "" else " " ++ unwords (map display ms)) ++
    maybe "" (\r -> " returns "++display r) mtl'

  parser =
    do
      t <- parserBasic <* whitespace
      mes <- many (parseArrayBrackets <* whitespace)
      return (construct t mes)
    where
      parserBasic = choice
        [ do
            t1 <- keyword "mapping" *> whitespace *> char '(' *> parser <* whitespace <* string "=>" <* whitespace
            t2 <- parser <* whitespace <* char ')'
            return (TypeNameMapping t1 t2)
        , try $ TypeNameElementaryTypeName <$> parser
        , try $ do
            tl <- keyword "function" *> whitespace *> parser <* whitespace
            ms <- many (parser <* whitespace)
            mtl' <- try (Just <$> keyword "returns" *> whitespace *> parser) <|> return Nothing
            return (TypeNameFunctionTypeName tl ms mtl')
        , TypeNameUserDefinedTypeName <$> parser
        ]

      parseArrayBrackets =
        char '[' *> whitespace *>
        do
          me <- (char ']' *> return Nothing) <|> (Just <$> parser <* whitespace <* char ']')
          return me

      construct t [] = t
      construct t (me:mes) = construct (TypeNameArrayTypeName t me) mes

-------------------------------------------------------------------------------
-- UserDefinedTypeName = Identifier ( '.' Identifier )*

instance Parseable UserDefinedTypeName where
  display (UserDefinedTypeName is) = intercalate "." (map display is)
  parser = UserDefinedTypeName <$> sep1 '.' parser

-------------------------------------------------------------------------------
-- StorageLocation = 'memory' | 'storage'

instance Parseable StorageLocation where
  display Memory = "memory"
  display Storage = "storage"
  display CallData = "calldata"
  parser = (const Memory <$> keyword "memory") 
       <|> (const Storage <$> keyword "storage") 
       <|> (const Storage <$> keyword "calldata")

-------------------------------------------------------------------------------
-- StateMutability = 'internal' | 'external' | 'pure' | 'constant' | 'view' | 'payable'

instance Parseable StateMutability where
  display Pure = "pure"
  display Internal = "internal"
  display External = "external"
  display Constant = "constant"
  display View = "view"
  display Payable = "payable"

  parser = choice
    [ const Internal <$> keyword "internal"
    , const External <$> keyword "external"
    , const Constant <$> keyword "constant"
    , const View <$> keyword "view"
    , char 'p' *>
        ((const Pure <$> keyword "ure") <|> (const Payable <$> keyword "ayable"))
    ]

-------------------------------------------------------------------------------
-- IdentifierList = '(' ( Identifier? ',' )* Identifier? ')'

instance Parseable IdentifierList where
  display (IdentifierList is) =
      "(" ++ intercalate ", " (map display is) ++ ")"
  parser =
    IdentifierList <$> (char '(' *> whitespace *> commaSep (parser <* whitespace) <* char ')')

-------------------------------------------------------------------------------
-- Block = '{' Statement* '}'

instance Parseable Block where
  display (Block ss) = "{\n"++indent (intercalate "\n" (map display ss))++"}"
  parser = Block <$> (char '{' *> whitespace *> many (parser <* whitespace) <* char '}')


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

instance Parseable Statement where
  display (IfStatement e t me) = "if ("++display e++") "++display t++maybe "" (\s -> if s /= BlockStatement (Block []) then " else "++display s else "") me
  display (WhileStatement e s) = "while (" ++ display e++") "++display s
  display (InlineAssemblyStatement ms b) = "assembly "++maybe " " (\s -> display s++" ") ms++display b
  display (ForStatement (ms, me1, me2) s) =
    "for ("++
      maybe "; " (\s -> display s ++" ") ms ++
      maybe "" display me1 ++"; "++
      maybe "" display me2 ++
    ") "++display s
  display (BlockStatement b) = display b

  display (DoWhileStatement s e) = "do "++display s++" while ("++display e++");"
  display PlaceholderStatement = "_;"
  display Continue = "continue;"
  display Break = "break;"
  display (Return me) = "return"++maybe "" (\e -> " "++display e) me++";"
  display Throw = "throw;"
  display (EmitStatement exp) = "emit "++(display exp)++";"

  display (SimpleStatementExpression e) = display e++";"
  display (SimpleStatementVariableList il me) = "var " ++ display il ++ maybe "" (\e -> " = "++display e) me++";"
  -- display (SimpleStatementVariableDeclaration v me) = display v ++ maybe "" (\e -> " = "++display e) me ++";"
  display (SimpleStatementVariableDeclarationList [v] []) = display v ++";"
  display (SimpleStatementVariableDeclarationList [v] [d]) = display v ++ " = " ++ display d ++";"
  display (SimpleStatementVariableDeclarationList vs []) = "(" ++ intercalate ", " (map display vs) ++ ")" ++";"
  display (SimpleStatementVariableDeclarationList vs me) = "(" ++ intercalate ", " (map display vs) ++ ")" ++ " = " ++ "(" ++ intercalate ", " (map display me) ++ ")" ++";"
  
  display (SimpleStatementVariableAssignmentList [v] []) = display v ++";"
  display (SimpleStatementVariableAssignmentList [v] [d]) = display v ++ " = " ++ display d ++";"
  display (SimpleStatementVariableAssignmentList vs []) = "(" ++ intercalate ", " (map display vs) ++ ")" ++";"
  display (SimpleStatementVariableAssignmentList vs me) = "(" ++ intercalate ", " (map display vs) ++ ")" ++ " = " ++ "(" ++ intercalate ", " (map display me) ++ ")" ++";"

  parser =
    try (choice
      [ do
          s <- keyword "do" *> whitespace *> parser <* whitespace <* keyword "while" <* whitespace <* char '(' <* whitespace
          e <- parser <* whitespace <* char ')' <* whitespace <* char ';'
          return (DoWhileStatement s e)
      , const PlaceholderStatement <$> (char '_' <* whitespace <* char ';')
      , const Continue <$> (keyword "continue" <* whitespace <* char ';')
      , const Break <$> (keyword "break" <* whitespace <* char ';')
      , const Throw <$> (keyword "throw" <* whitespace <* char ';')
      , EmitStatement <$> (keyword "emit" *> whitespace *> parser <* whitespace <* char ';')
      , Return <$> (keyword "return" *> whitespace *> ((Just <$> parser) <|> return Nothing) <* whitespace <* char ';')
      , do
          c <- keyword "if" *> whitespace *> char '(' *> whitespace *> parser <* whitespace <* char ')' <* whitespace
          t <- parser <* whitespace
          e <- (Just <$> keyword "else" *> whitespace *> parser) <|> return Nothing
          return (IfStatement c t e)
      , do
          c <- keyword "while" *> whitespace *> char '(' *> whitespace *> parser <* whitespace <* char ')' <* whitespace
          s <- parser
          return (WhileStatement c s)
      , BlockStatement <$> parser
      , do
          n <- keyword "assembly" *> whitespace *> ((Just <$> parser) <|> return Nothing) <* whitespace
          b <- parser
          return (InlineAssemblyStatement n b)
      , do
          s1 <- keyword "for" *> whitespace *> char '(' *> whitespace *>
                  (try (Just <$> parseSimpleStatement) <|> (char ';' *> return Nothing)) <* whitespace
          s2 <- (try (Just <$> parser) <|> return Nothing) <* whitespace <* char ';' <* whitespace
          s3 <- (try (Just <$> parser) <|> return Nothing) <* whitespace <* char ')' <* whitespace
          b <- parser
          return (ForStatement (s1,s2,s3) b)
      ]
    ) <|> parseSimpleStatement
    where
      parseSimpleStatement =
        try (
          do
            il <- keyword "var" *> whitespace *> parser <* whitespace <* char '=' <* whitespace
            me <- try (Just <$> parser) <|> return Nothing
            _  <- whitespace *> char ';'
            return (SimpleStatementVariableList il me)
          )
        <|>
        -- try (
        --   do
        --     vd <- try (char '(' *> whitespace *> parser <* whitespace <* char ')') <|> parser
        --     whitespace
        --     me <- try (Just <$> (char '=' *> whitespace *> parser)) <|> return Nothing
        --     _  <- whitespace *> char ';'
        --     return (SimpleStatementVariableDeclaration vd me)
        --   )
        -- <|>
        try (
          do
            char '('
            whitespace
            char ')'
            whitespace
            char ';'
            return (SimpleStatementVariableDeclarationList [] [])
          )
        <|>
        try (
          do
            optional (char '(') <* whitespace
            vd <- commaSep1 (try (Just <$> whitespace *> parser <* whitespace) <|> (char ' ' *> whitespace *> return Nothing))
            optional (char ')') <* whitespace
            r <- try( do char '=' <* whitespace
                         me <- (whitespace *> parser <* whitespace)
                         whitespace <* char ';'
                         return [me])
                  <|>
                  try( do char '=' <* whitespace
                          optional (char '(') 
                          mes <- try (commaSep1 (whitespace *> parser <* whitespace)) <|> return [] 
                          optional (char ')')
                          whitespace <* char ';'
                          return mes)
                  <|> return [] <$> whitespace <* char ';'
            return (SimpleStatementVariableDeclarationList vd r)
          )
        <|>
        try (
          do
            optional (char '(') <* whitespace
            vd <- commaSep1 (try (Just <$> whitespace *> parser <* whitespace) <|> (char ' ' *> whitespace *> return Nothing))
            optional (char ')') <* whitespace
            r <- try( do char '=' <* whitespace
                         me <- (whitespace *> parser <* whitespace)
                         whitespace <* char ';'
                         return [me])
                  <|>
                  try( do char '=' <* whitespace
                          optional (char '(') 
                          mes <- try (commaSep1 (whitespace *> parser <* whitespace)) <|> return [] 
                          optional (char ')')
                          whitespace <* char ';'
                          return mes)
                  <|> return [] <$> whitespace <* char ';'
            return (SimpleStatementVariableAssignmentList vd r)
          )
        <|>
        SimpleStatementExpression <$> parser <* whitespace <* char ';'

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

instance Parseable Expression where
  display (New t) = "new "++display t
  display (MemberAccess e i) = display e++"."++display i
  display (Literal e) = display e
  display (FunctionCallNameValueList e mvs) = display e ++ "({"++maybe "" display mvs++"})"
  display (FunctionCallExpressionList e mvs) = display e ++ "("++maybe "" display mvs++")"

  display (Unary "delete" e) = "delete "++display e
  display (Unary "()++" e) = display e++"++"
  display (Unary "()--" e) = "("++display e++")--"
  display (Unary "()" e) = "("++display e++")"
  display (Unary "[]" e) = display e++"[]"
  -- Remaining: ! ~ + - ++ --
  display (Unary op e) = op++display e

  display (Binary "[]" e1 e2) = display e1 ++ "[" ++ display e2++"]"
  -- Remaining = |= ^= &= <<= >>= += -= *= /= %= || && == != <= >= < > + - * ** / % ^ | & >> <<
  display (Binary op e1 e2) = display e1 ++ " " ++ op ++ " " ++ display e2

  display (Ternary "?" e1 e2 e3) = display e1 ++"?"++display e2++":"++display e3

  parser = parserPrec 15
    where
      anyString ss = choice (map (try . string) (init ss) ++ [string $ last ss])
      binaryOperators n ops =
          do
            p1 <- parserPrec (n-1) <* whitespace
            try (
              do
                op <- anyString ops <* whitespace
                p2 <- parserPrec n
                return (Binary op p1 p2)
              ) <|> return p1


      parserPrec 15 = binaryOperators 15 ["=", "|=", "^=", "&=", "<<=", ">>=", "+=", "-=", "*=", "/=", "%="]
      parserPrec 14 =
        try (
          do
            p1 <- parserPrec 13 <* whitespace
            p2 <- char '?' *> whitespace *> parserPrec 13 <* whitespace
            p3 <- char ':' *> whitespace *> parserPrec 13
            return (Ternary "?" p1 p2 p3)
          ) <|> parserPrec 13
      parserPrec 13 = binaryOperators 13 ["||"]
      parserPrec 12 = binaryOperators 12 ["&&"]
      parserPrec 11 = binaryOperators 11 ["==","!="]
      parserPrec 10 = binaryOperators 10 ["<=", ">=", "<", ">"]
      parserPrec 9 = binaryOperators 9 ["|"]
      parserPrec 8 = binaryOperators 8 ["^"]
      parserPrec 7 = binaryOperators 7 ["&"]
      parserPrec 6 = binaryOperators 6 ["<<", ">>"]
      parserPrec 5 = binaryOperators 5 ["+", "-"]
      parserPrec 4 = binaryOperators 4 ["*", "/", "%"]
      parserPrec 3 = binaryOperators 3 ["**"]
      parserPrec 2 =
        try (
          do
            op <- (anyString ["++", "--", "+", "-", "!", "~"] <|> keyword "delete") <* whitespace
            p <- parserPrec 15
            return (Unary op p)
          ) <|> parserPrec 1

      -- parserPrec 1 split into two to avoid infinite loops
      parserPrec 1 = choice
          [ try $ do
              p <- parserPrec 0 <* whitespace
              op <- anyString ["++","--"]
              return (Unary ('(':')':op) p)
          ] <|> parserPrec 0

      parserPrec 0 =
        do
          p1 <- parserPrecBasic <* whitespace
          p <- addBottomLevelOperators p1
          return p
        where
          addBottomLevelOperators :: Expression -> Parser Expression
          addBottomLevelOperators p1 =
            choice
              [ try $ do
                  p2 <- char '[' *> whitespace *> parserPrec 15 <* whitespace <* char ']'
                  p  <- addBottomLevelOperators (Binary "[]" p1 p2)
                  return p
              , try (
                  do
                    i <- char '.' *> whitespace *> parser
                    p  <- addBottomLevelOperators (MemberAccess p1 i)
                    return p
                  )
              , try (
                  do
                    _ <- char '(' <* whitespace
                    result <- (
                        (char '{' *> whitespace *> (FunctionCallNameValueList p1 <$> (parser <* whitespace <* char '}' <* whitespace <* char ')'))) <|>
                        (FunctionCallExpressionList p1 <$> (parser <* whitespace <* char ')'))
                      )
                    p  <- addBottomLevelOperators (result)
                    return p
                  )
              , return p1
              ]
          parserPrecBasic :: Parser Expression
          parserPrecBasic =
            choice
              [ try $ New <$> (keyword "new" *> whitespace *> parser)
              , try $ Unary "()" <$> (char '(' *> whitespace *> parserPrec 15 <* whitespace <* char ')')
              , Literal <$> parser
              ]


-------------------------------------------------------------------------------
-- PrimaryExpression = BooleanLiteral
--                   | NumberLiteral
--                   | HexLiteral
--                   | StringLiteral
--                   | TupleExpression
--                   | Identifier
--                   | ElementaryTypeNameExpression

instance Parseable PrimaryExpression where
  parser = choice
    [ try $ PrimaryExpressionBooleanLiteral <$> parser
    , try $ PrimaryExpressionNumberLiteral <$> parser
    , try $ PrimaryExpressionHexLiteral <$> parser
    , try $ PrimaryExpressionStringLiteral <$> parser
    , try $ PrimaryExpressionTupleExpression <$> parser
    , try $ PrimaryExpressionIdentifier <$> parser
    , PrimaryExpressionElementaryTypeNameExpression <$> parser
    ]
  display (PrimaryExpressionBooleanLiteral l) = display l
  display (PrimaryExpressionNumberLiteral l) = display l
  display (PrimaryExpressionHexLiteral l) = display l
  display (PrimaryExpressionStringLiteral l) = display l
  display (PrimaryExpressionTupleExpression l) = display l
  display (PrimaryExpressionIdentifier l) = display l
  display (PrimaryExpressionElementaryTypeNameExpression l) = display l

-------------------------------------------------------------------------------
-- ExpressionList = Expression ( ',' Expression )*

instance Parseable ExpressionList where
  parser = ExpressionList <$> commaSep1 parser
  display (ExpressionList es) = intercalate ", " (map display es)

-------------------------------------------------------------------------------
-- NameValueList = Identifier ':' Expression ( ',' Identifier ':' Expression )*

instance Parseable NameValueList where
  parser =
    NameValueList <$> commaSep1 (
      do {i <- parser <* whitespace <* char ':' <* whitespace; e <- parser; return (i,e) }
    )
  display (NameValueList ies) = intercalate ", " $ map (\(i,e) -> display i ++":"++display e) ies
-------------------------------------------------------------------------------
-- BooleanLiteral = 'true' | 'false'

instance Parseable BooleanLiteral where
  parser = BooleanLiteral <$> (string "true" <|> string "false")
  display (BooleanLiteral lit) = lit

-------------------------------------------------------------------------------
-- NumberLiteral = ( HexNumber | DecimalNumber ) (' ' NumberUnit)?

-- HexNumber = '0x' [0-9a-fA-F]+
-- DecimalNumber = [0-9]+

instance Parseable NumberLiteral where
  parser =
     try (do
        n <- string "0x" *> many1 (digit <|> oneOf "ABCDEFabcdef")
        u <- parseMaybeUnits
        return (NumberLiteralHex n u)
      )
    <|>
    do
      n <- many1 digit
      u <- parseMaybeUnits
      return (NumberLiteralDec n u)
    where
      parseMaybeUnits = try (Just <$> char ' ' *> whitespace *> parser) <|> return Nothing

  display (NumberLiteralHex n units) = "0x"++ n ++ maybe "" _display units
  display (NumberLiteralDec n units) = n ++ maybe "" _display units

-------------------------------------------------------------------------------
-- NumberUnit = 'wei' | 'szabo' | 'finney' | 'ether'
--           | 'seconds' | 'minutes' | 'hours' | 'days' | 'weeks' | 'years'

instance Parseable NumberUnit where
  parser = choice [
      kw "finney" Finney, kw "ether" Ether, kw "years" Years,
      kw "minutes" Minutes, kw "hours" Hours, kw "days" Days,
      char 's' *> (kw "econds" Seconds <|> kw "zabo" Szabo),
      string "we" *> (kw "eks" Weeks <|> kw "i" Wei)
    ]
    where
      kw s v = keyword s *> return v
  display = map toLower . show

-------------------------------------------------------------------------------
-- HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')

instance Parseable HexLiteral where
  parser = HexLiteral . concat <$> (string "hex" *>
      try (char '"' *> manyTill parseHexByte (char '"')) <|>
      (char '\'' *> manyTill parseHexByte (char '\''))
    )
    where
      parseHexChar = digit <|> oneOf "ABCDEFabcdef"
      parseHexByte = list [parseHexChar, parseHexChar]

  display (HexLiteral hl) = "hex'"++hl++"'"

-------------------------------------------------------------------------------
-- StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'

instance Parseable StringLiteral where
  parser = StringLiteral . concat <$> (try (char '"' *> manyTill character (char '"'))
                                       <|> (char '\'' *> manyTill character (char '\'')))
    where
      escape :: Parser String
      escape = list [char '\\', oneOf ['\\','\"','0','n','r','v','t','b','f','x','u']] -- all the characters which can be escaped

      nonEscape :: Parser Char
      nonEscape = noneOf ['\\','\"','\0','\n','\r','\v','\t','\b','\f']

      character :: Parser String
      character = try (return <$> nonEscape) <|> escape
  display (StringLiteral s) = '"':addEscapes s++"\""
    where
      addEscapes = concatMap addEscape
      addEscape '\n' = "\\n"
      addEscape '\0' = "\\0"
    --  addEscape '\\' = "\\\\"
      addEscape '"' = "\""
      addEscape '\r' = "\\r"
      addEscape '\v' = "\\v"
      addEscape '\t' = "\\t"
      addEscape '\b' = "\\b"
      addEscape '\f' = "\\f"
      addEscape c = [c]

-------------------------------------------------------------------------------
-- Identifier = [a-zA-Z_$] [a-zA-Z_$0-9]*

instance Parseable Identifier where
  parser = Identifier <$> (
      (:) <$>
        (letter <|> oneOf "_$") <*>
        many (alphaNum <|> oneOf "_$") <*
        endOfWord
    )
  display (Identifier ident) = ident

-- -------------------------------------------------------------------------------
-- TupleExpression = '(' ( Expression ( ',' Expression )*  )? ')'
--                 | '[' ( Expression ( ',' Expression )*  )? ']'

instance Parseable TupleExpression where
  parser =
    (RoundBrackets <$> (char '(' *> whitespace *> commaSep (parser <* whitespace) <* char ')')) <|>
    (SquareBrackets <$> (char '[' *> whitespace *> commaSep1 (parser <* whitespace) <* char ']'))
  display (RoundBrackets es) = "(" ++ intercalate ", " (map display es) ++ ")"
  display (SquareBrackets es) = "[" ++ intercalate ", " (map display es) ++ "]"

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

instance Parseable ElementaryTypeName where
  display AddressPayableType = "address payable"
  display AddressType = "address"
  display BoolType = "bool"
  display StringType = "string"
  display VarType = "var"
  display ByteType = "byte"
  display (IntType Nothing) = "int"
  display (IntType (Just n)) = "int"++show n
  display (UintType Nothing) = "uint"
  display (UintType (Just n)) = "uint"++show n
  display (BytesType Nothing) = "bytes"
  display (BytesType (Just n)) = "bytes"++show n
  display (FixedType Nothing) = "fixed"
  display (FixedType (Just (d1,d2))) = "fixed"++show d1++"x"++show d2
  display (UfixedType Nothing) = "ufixed"
  display (UfixedType (Just (d1,d2))) = "ufixed"++show d1++"x"++show d2

  parser = choice
    [ try (const AddressPayableType <$> keyword "address" <* whitespace <* keyword "payable") <|> const AddressType <$> keyword "address"
    , const StringType <$> keyword "string"
    , const VarType <$> keyword "var"
    , IntType <$> (string "int" *> parseIntSize)
    , FixedType <$> (string "fixed" *> parseFixedPair)
    , char 'u' *> (
        (UfixedType <$> (string "fixed" *> parseFixedPair)) <|>
        (UintType <$> (string "int" *> parseIntSize))
      )
    , char 'b' *> (
        (const BoolType <$> keyword "ool") <|>
        (string "yte" *> (
          (BytesType <$> (char 's' *> parseBytesSize)) <|>
          (return ByteType)
        ))
      )
    ]
    where
      parseIntSize :: Parser (Maybe Integer)
      parseIntSize =
        do
          ns <- many digit
          let n = read ns :: Integer
          if not (null ns) && n `mod` 8 == 0 && n >= 8 && n <= 256 then return (Just n) else mzero
        <|> return Nothing
      parseBytesSize =
        do
          ns <- many digit
          let n = read ns :: Integer
          if not (null ns) && n > 0 && n >= 1 && n <= 32 then return (Just n) else mzero
        <|> return Nothing
      parseFixedPair =
        try (
          do
            d1 <- many digit <* char 'x'
            d2 <- many digit
            if (null d1 || null d2) then mzero else return (Just (read d1, read d2))
        ) <|> return Nothing


-- -------------------------------------------------------------------------------
-- InlineAssemblyBlock = '{' AssemblyItem* '}'

instance Parseable InlineAssemblyBlock where
  display (InlineAssemblyBlock is) = "{ "++unlines (map display is)++" }"
  parser = InlineAssemblyBlock <$> (char '{' *> whitespace *> many (parser <* whitespace) <* char '}')


-- -------------------------------------------------------------------------------
-- AssemblyItem = Identifier | FunctionalAssemblyExpression | InlineAssemblyBlock | AssemblyLocalBinding | AssemblyAssignment | AssemblyLabel | NumberLiteral | StringLiteral | HexLiteral
-- AssemblyLabel = Identifier ':'
-- AssemblyLocalBinding = 'let' Identifier ':=' FunctionalAssemblyExpression
-- AssemblyAssignment = ( Identifier ':=' FunctionalAssemblyExpression ) | ( '=:' Identifier )

instance Parseable AssemblyItem where
  parser = choice
    [ try $ AssemblyItemFunctionalAssemblyExpression <$> parser
    , try $ AssemblyItemInlineAssemblyBlock <$> parser
    , try $ do { _ <- keyword "let" <* whitespace; i <- parser <* whitespace <* string ":=" <* whitespace; e <- parser; return (AssemblyItemAssemblyLocalBinding i e) }
    , try $ do { i <- parser <* whitespace <* string ":=" <* whitespace; e <- parser; return (AssemblyItemAssemblyAssignment i e) }
    , try $ do { e <- parser <* whitespace <* string "=:" <* whitespace; i <- parser; return (AssemblyItemAssemblyAssignment i e) }
    , try $ AssemblyItemAssemblyLabel <$> parser <* char ':'
    , try $ AssemblyItemNumberLiteral <$> parser
    , try $ AssemblyItemStringLiteral <$> parser
    , try $ AssemblyItemHexLiteral <$> parser
    , AssemblyItemIdentifier <$> parser
    ]
  display (AssemblyItemStringLiteral s) = display s
  display (AssemblyItemHexLiteral h) = display h
  display (AssemblyItemNumberLiteral n) = display n
  display (AssemblyItemAssemblyLabel l) = display l++":"
  display (AssemblyItemIdentifier i) = display i
  display (AssemblyItemAssemblyLocalBinding i e) = "let "++display i++" := "++display e
  display (AssemblyItemAssemblyAssignment i e) = display i++" := "++display e
  display (AssemblyItemInlineAssemblyBlock b) = display b
  display (AssemblyItemFunctionalAssemblyExpression e) = display e


-- -------------------------------------------------------------------------------
-- FunctionalAssemblyExpression = Identifier '(' AssemblyItem? ( ',' AssemblyItem )* ')'

instance Parseable FunctionalAssemblyExpression where
  parser =
    do
      i <- parser <* whitespace <* char '(' <* whitespace
      items <- commaSep (parser <* whitespace) <* char ')'
      return (FunctionalAssemblyExpression i items)

  display (FunctionalAssemblyExpression i items) = display i ++ "(" ++ intercalate ", " (map display items) ++ ")"

