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

module DEA.DEA (
    Event (..), FunctionCall (..), isControlFlowEvent, isDataFlowEvent,
    getFunctionNameFromEvent, getVariableNameFromEvent,
    GCL (..), State (..), Transition (..),
    DEA (..), getEventsFromDEA, getFunctionsFromDEA, initialState,

    getVariablesFromDEA, getVariablesFromContractSpecification,

    functionCallEventHasParameters, getFunctionParametersFromEvent,
    problemsSpecification, problemsContractSpecification, problemsDEA,
    ContractSpecification (..), Specification (..),
    module Solidity
  ) where

import Data.List
import Data.Maybe

import Solidity

data Event
  = UponEntry FunctionCall
  | UponExit FunctionCall
  | VariableAssignment VariableName (Maybe Expression)
  deriving (Eq, Ord, Show)

data FunctionCall =
  FunctionCall {
    functionName :: FunctionName,
    parametersPassed :: Maybe UntypedParameterList
  } deriving (Eq, Ord, Show)

isControlFlowEvent :: Event -> Bool
isControlFlowEvent (VariableAssignment _ _) = False
isControlFlowEvent _ = True

isDataFlowEvent :: Event -> Bool
isDataFlowEvent = not . isControlFlowEvent

getFunctionNameFromEvent :: Event -> FunctionName
getFunctionNameFromEvent (UponEntry fc) = functionName fc
getFunctionNameFromEvent (UponExit fc) = functionName fc

functionCallEventHasParameters :: Event -> Bool
functionCallEventHasParameters (UponEntry fc) = isJust (parametersPassed fc)
functionCallEventHasParameters (UponExit fc) = isJust (parametersPassed fc)

getFunctionParametersFromEvent :: Event -> UntypedParameterList
getFunctionParametersFromEvent (UponEntry fc) = fromJust (parametersPassed fc)
getFunctionParametersFromEvent (UponExit fc) = fromJust (parametersPassed fc)

getVariableNameFromEvent :: Event -> VariableName
getVariableNameFromEvent (VariableAssignment v _) = v

data GCL = GCL {
    event :: Event,
    guard :: Maybe Expression,
    action :: Maybe Statement
} deriving (Eq, Ord, Show)

newtype State = State { unState :: String } deriving (Eq, Ord, Show)

data Transition =
  Transition {
      src, dst :: State,
      label :: GCL
} deriving (Eq, Ord, Show)

data DEA = DEA {
  deaName :: String,
  allStates :: [State],
  initialStates :: [State],
  transitions :: [Transition],
  badStates :: [State],
  acceptanceStates :: [State]
} deriving (Eq, Ord, Show)

data ContractSpecification = ContractSpecification {
  contractName :: ContractName,
  declarations :: [ContractPart],
  initialisation :: Block,
  satisfaction :: Block,
  reparation :: Block,
  deas :: [DEA]
} deriving (Eq, Ord, Show)

newtype Specification = Specification { contractSpecifications :: [ContractSpecification] } deriving (Eq, Ord, Show)

initialState :: DEA -> State
initialState = head . initialStates

getEventsFromDEA :: DEA -> [Event]
getEventsFromDEA = nub . map (event . label) . transitions

getFunctionsFromDEA :: DEA -> [FunctionName]
getFunctionsFromDEA = nub . map getFunctionNameFromEvent . filter isControlFlowEvent . map (event . label) . transitions

getVariablesFromDEA :: DEA -> [VariableName]
getVariablesFromDEA = nub . map getVariableNameFromEvent . filter isDataFlowEvent . map (event . label) . transitions

getVariablesFromContractSpecification :: ContractSpecification -> [VariableName]
getVariablesFromContractSpecification =
  nub . map getVariableNameFromEvent . filter isDataFlowEvent . map (event . label) . concat . map transitions . deas


problemsSpecification :: Specification -> [String]
problemsSpecification spec =
  [ "Error: Multiple specifications for contract <"++display c++">" | c <- nub cnames, length (filter (c==) cnames) > 1 ] ++
  concat (map problemsContractSpecification cspecs)
  where
    cspecs = contractSpecifications spec
    cnames = map contractName cspecs

problemsContractSpecification :: ContractSpecification -> [String]
problemsContractSpecification cspec
  | null problems = []
  | otherwise = ("Errors in definition of specification of contract <"++display (contractName cspec)++">"):problems
  where
    problems =
      [ "  Multiple DEAs named <"++d++">"
      | d <- nub dnames, length (filter (d==) dnames) > 1
      ] ++
      concat
      [ ("  In definition of DEA <"++deaName dea++">"):map ("     - "++) ps
      | dea <- deas cspec
      , let ps = problemsDEA dea
      , not (null ps)
      ]
      where
        dnames = map deaName (deas cspec)

problemsDEA :: DEA -> [String]
problemsDEA dea =
  [ "DEA has no initial state" | length (initialStates dea) == 0 ] ++
  [ "DEA has more than one initial state" | length (initialStates dea) > 1 ] ++
  [ "State <"++unState s++"> defined multiple times"
  | s <- nub (allStates dea), length (filter (s==) (allStates dea)) > 1
  ] ++
  [ "State <"++unState s++"> used in a transition but not declared"
  | s <- tstates, s `notElem` allStates dea
  ]
  where
    tstates = nub $ concat [ [src transition, dst transition] | transition <- transitions dea ]


