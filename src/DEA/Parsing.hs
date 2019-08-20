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

module DEA.Parsing (
  module Parseable, module DEA.DEA
) where

import Data.List
import Control.Monad hiding (guard)
import Text.Parsec hiding (State, label)
import Text.Parsec.String

import Parseable
import Solidity
import DEA.DEA

-- ------------------------------------
readComment :: Parser ()
readComment =
  try (string "//" *> manyTill anyChar (eof <|> (newline *> return ())) *> return ()) <|>
  (string "/*" *> manyTill anyChar (string "*/") *> return ())

whitespace :: Parser ()
whitespace  = void $ many ((space *> return ()) <|> readComment)

endOfWord :: Parser ()
endOfWord = eof <|> notFollowedBy (alphaNum <|> char '_')

readIdentifier :: Parser String
readIdentifier = (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_') <* endOfWord

readKeyword :: String -> Parser String
readKeyword word = string word <* endOfWord

readAnyKeyword :: Parser String
readAnyKeyword = many letter <* endOfWord

commaSep1 :: Parser a -> Parser [a]
commaSep1 p =
  do
    x <- p <* whitespace
    xs <- many (char ',' *> whitespace *> p <* whitespace)
    return (x:xs)


instance Parseable Specification where
  display = intercalate "\n" . map display . contractSpecifications
  parser = whitespace *> (Specification <$> (many parser <* whitespace)) <* eof

instance Parseable ContractSpecification where
  display monitor = unlines $
    [ "monitor " ++ display (contractName monitor) ++ " {"
    , "declarations {"
    ] ++ map display (declarations monitor) ++
    [ "}"
    , "initialisation ", display (initialisation monitor)
    , "reparation ", display (reparation monitor)
    , "satisfaction ", display (satisfaction monitor)
    ] ++ map display (deas monitor) ++ ["}"]
    where
      indentLineList = map ("   "++)
      indentLines line = concat [ if (c=='\n') then "\n   " else [c] | c <- line ]

  parser =
    do
      _contractName <- readKeyword "monitor" *> whitespace *> parser <* whitespace <* char '{' <* whitespace
      _declarations <-
        (try (
          readKeyword "declarations" *> whitespace *> char '{' *> whitespace *> many (parser <* whitespace) <* char '}'
        ) <|> return []) <* whitespace
      _initialisation <-
        (try (readKeyword "initialisation" *> whitespace *> parser) <|> return (Block [])) <* whitespace
      _reparation <-
        (try (readKeyword "reparation" *> whitespace *> parser) <|> return (Block [])) <* whitespace
      _satisfaction <-
        (try (readKeyword "satisfaction" *> whitespace *> parser) <|> return (Block [])) <* whitespace
      _deas <- many (parser <* whitespace)
      _ <- char '}'
      return $ ContractSpecification {
        contractName = _contractName,
        declarations = _declarations,
        initialisation = _initialisation,
        satisfaction = _satisfaction,
        reparation = _reparation,
        deas = _deas
      }

instance Parseable DEA where
  parser =
    do
      _deaName <- readKeyword "DEA" *> whitespace *> readIdentifier <* whitespace <* char '{' <* whitespace
      (_allStates, _initialStates, _badStates, _acceptanceStates) <- readStates <* whitespace
      _transitions <-
        readKeyword "transitions" *> whitespace *> char '{' *> whitespace *>
        many (parser <* whitespace) <* char '}' <* whitespace
      _ <- char '}' <* whitespace
      return DEA {
          deaName = _deaName,
          allStates = _allStates,
          initialStates = _initialStates,
          transitions = _transitions,
          badStates = _badStates,
          acceptanceStates = _acceptanceStates
        }
    where
      readStates =
        processStates <$> (
            readKeyword "states" *> whitespace *> char '{' *> whitespace *>
            many (
                do
                  s <- parser <* whitespace
                  t <- ((char ':' *> whitespace *> readStateTag) <|> return "") <* whitespace <* char ';' <* whitespace
                  return (s,t)
              ) <* char '}'
          )
        where
          processStates :: [(State, String)] -> ([State],[State], [State], [State])
          processStates stateList = (allStates, initialStates, badStates, acceptanceStates)
            where
              allStates = map fst stateList
              initialStates = [ s | (s,"initial") <- stateList ]
              badStates = [ s | (s,"bad") <- stateList ]
              acceptanceStates = [ s | (s,"accept") <- stateList ]

  display dea =
    unlines $
      [ "DEA "++deaName dea ++" {"
      , "   states {"
      ] ++
      [ "     " ++ display state ++ describe state ++ ";"
      | state <- allStates dea
      ] ++
      [ "   }"
      , "   transitions {"
      ] ++
      [ "     " ++ display t++";" | t <- transitions dea ] ++
      [ "   }"
      , "}"
      ]
    where
      describe state
        | null keywords = ""
        | otherwise = ": "++intercalate ", " keywords
        where
          keywords =
            ["initial" | state == initialState dea]++
            ["bad" | state `elem` badStates dea]++
            ["accept" | state `elem` acceptanceStates dea]


instance Parseable State where
  parser = State <$> readAnyKeyword
  display = unState

readStateTag :: Parser String
readStateTag = choice $ map readKeyword ["initial", "bad", "accept"]

instance Parseable Transition where
  parser =
    do
      _src <- parser <* whitespace
      _label <- string "-[" *> whitespace *> parser <* whitespace <* string "]->" <* whitespace
      _dst <- parser <* whitespace <* char ';'
      return Transition { src = _src, dst = _dst, label = _label }
  display transition =
    display (src transition)++" -["++display (label transition)++"]-> "++display (dst transition)

instance Parseable GCL where
  parser =
    do
      _event <- parser <* whitespace
      _guard <-
          ( try (Just <$> (char '|' *> whitespace *> parser)) <|>
            try (const Nothing <$> char '|') <|>
            return Nothing
          ) <* whitespace
      _action <-
          ( try (Just <$> (string "~>" *> whitespace *> parser)) <|>
            try (const Nothing <$> string "~>") <|>
            return Nothing
          )
      return GCL { event = _event, guard = _guard, action = _action }
  display rule = concat $
    [ display (event rule) ] ++
    [ " | (" ++display c++")" | Just c <- [guard rule] ] ++
    [ " ~> {"++lineDisplay a++"}" | Just a <- [action rule]]


instance Parseable Event where
  parser =
    try (UponEntry <$> (readKeyword "before" *> whitespace *> char '(' *> whitespace *> parser <* whitespace <* char ')')) <|>
    try (UponExit <$> (readKeyword "after" *> whitespace *> char '(' *> whitespace *> parser <* whitespace <* char ')')) <|>
    (VariableAssignment <$>
        (parser <* whitespace <* char '@' <* whitespace <* char '(' <* whitespace) <*>
        (try (Just <$> parser <* whitespace <* char ')') <|> return Nothing)
    )

  display (UponEntry fn) = "before("++display fn++")"
  display (UponExit fn) = "after("++display fn++")"
  display (VariableAssignment vn e) = display vn ++maybe "" (\e -> "@("++display e++")") e

instance Parseable FunctionCall where
  parser =
    do
      fn <- parser <* whitespace
      maybe_el <- parser
      return (FunctionCall { functionName = fn, parametersPassed = maybe_el })

  display fc = display (functionName fc) ++ maybe "" display (parametersPassed fc)

