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

module Parseable (Parseable (..), _display, lineDisplay) where

import Data.Char
import Text.Parsec
import Text.Parsec.String

class Parseable a where
  display :: a -> String
  parser :: Parser a

instance Parseable a => Parseable (Maybe a) where
  parser = (Just <$> try parser) <|> return Nothing
  display Nothing = ""
  display (Just x) = display x

_display :: Parseable a => a -> String
_display = (' ':) . display

lineDisplay :: Parseable a => a -> String
lineDisplay = filter (not . isSpace) . display
