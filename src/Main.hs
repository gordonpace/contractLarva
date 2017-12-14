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

module Main where

import System.Environment
import System.Exit
import Control.Exception.Base
import System.IO
import System.IO.Error

import Text.Parsec hiding (try)
import Text.Parsec.String
import Solidity
import DAE

type Filename = String

failWith :: IO a -> String -> IO a
io `failWith` e = io `catch` (const $ (fail e) :: IOError -> IO a)

ifNot :: Bool -> String -> IO ()
ifNot c e = if c then return () else fail e

parseIO :: Parseable a => Filename -> String -> IO a
parseIO filename = either (fail . (parseError ++) . show) return . parse parser "" 
  where
    parseError = "Error during parsing of <"++filename++">\n"

main = 
  do 
    contractLarva <- getProgName
    arguments <- getArgs
    ifNot (length arguments == 3)  
      ("Usage: "++contractLarva++" <specification> <input solidity file> <output solidity file>")
    let [specificationFile, inFile, outFile] = arguments 

    specificationText <- readFile specificationFile 
      `failWith` ("Cannot read specification file <"++specificationFile++">")
    specification <- parseIO specificationFile specificationText
    let problems = problemsSpecification specification
    ifNot (null problems) (unlines problems)

    inputText <- readFile inFile 
      `failWith` ("Cannot read Solidity file <"++inFile++">")
    inCode <- parseIO inFile inputText
    let outCode = instrumentSpecification specification inCode
    writeFile outFile (display outCode) 
      `failWith` ("Cannot write to Solidity file <"++outFile++">")
    putStrLn ("Created safe contract file <"++outFile++">")
  `catch` (putStrLn . ioeGetErrorString)



