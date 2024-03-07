import Data.List (elemIndex)
import Data.Maybe (fromMaybe, Maybe(Just))

data Rule = Rule {
  index  :: !Int,
  dest   :: !Int,
  input  :: !Char,
  output :: !Char
} deriving (Show)

newRule :: Int -> Int -> Char -> Char -> Rule
newRule index dest input output = Rule{index, dest, input, output}

ruleList :: [Rule]
ruleList = [
  newRule 1  2  '1' 'L',
  newRule 2  3  '0' 'L',
  newRule 2  3  '1' 'L',
  newRule 3  3  '0' '1',
  newRule 3  4  '1' 'L',
  newRule 4  4  '0' '1',
  newRule 4  5  '1' 'R',
  newRule 5  5  '1' 'R',
  newRule 5  6  '0' 'R',
  newRule 6  6  '1' 'R',
  newRule 6  7  '0' 'L',
  newRule 7  7  '1' '0',
  newRule 7  8  '0' 'L',
  newRule 8  11 '0' 'L',
  newRule 11 11 '1' 'L',
  newRule 11 12 '0' 'R',
  newRule 8  9  '1' 'L',
  newRule 9  9  '1' 'L',
  newRule 9  10 '0' 'L',
  newRule 10 2  '0' 'R']

data TapeMachine = TapeMachine{
  states :: ![Char],
  pos    :: !Int,
  offset :: !Int, -- The offset is the state machine position that corresponds to 0 pos in states list. Allways 0 or less
  rule   :: !Int
} deriving (Show)

tapeMachine :: [Char] -> Int -> Int -> Int -> TapeMachine
tapeMachine states pos offset rule = TapeMachine{
  states,
  pos,
  offset,
  rule
}

-- Program has terminated if the rule index cant be found
programTerminated :: Int -> Bool
programTerminated output_index = output_index `elem` map index ruleList

nextRule :: Int -> Int
nextRule rule_index = index $ ruleList !! fromMaybe (-1) (rule_index `elemIndex` map dest ruleList)

newTapeMachine :: [Char] -> TapeMachine
newTapeMachine states = tapeMachine states 0 0 (index $ head ruleList)

-- Where the relative position of the end of states is
end :: TapeMachine -> Int
end machine = length (states machine) + offset machine

-- push functions simulate the list being infinate
beforeOffset, afterEnd :: Int -> TapeMachine -> Bool
beforeOffset new_pos machine = new_pos < offset machine
afterEnd new_pos machine = end machine >= new_pos

push :: Int -> ([Char] -> [Char]) -> (TapeMachine -> TapeMachine)
push pos_delta listAppend machine
  = tapeMachine (listAppend $ states machine) (pos machine) (offset machine + pos_delta) (rule machine)

infix 5 +:
(+:) :: [a] -> a -> [a]
l +: v = l ++ [v]

pushLeft, pushRight :: TapeMachine -> TapeMachine
pushLeft = push (-1) ('0':)
pushRight = push 0 (+:'0')

-- functions for simulating turing machine operations
move :: Int -> (Int -> TapeMachine -> Bool) -> (TapeMachine -> TapeMachine) -> (TapeMachine -> TapeMachine)
move pos_delta pushComp pushFunc machine =
  let _states = states machine in
  let _offset = offset machine in
  let _rule   = rule machine in
  let new_pos = pos machine + pos_delta in
  if pushComp new_pos machine then
    pushFunc $ tapeMachine _states new_pos _offset _rule
  else
    tapeMachine _states new_pos _offset _rule

moveLeft, moveRight :: TapeMachine -> TapeMachine
moveLeft = move (-1) beforeOffset pushLeft
moveRight = move 1 afterEnd pushRight

-- functions for swapping a value
-- to get arround Haskell list imutability a new list is processed
changeAtPos :: [a] -> Int -> a -> [a]
changeAtPos list index value =
  let atPos i x = if i == index then value else x in
  zipWith atPos [0..] list

changeMachineVal :: Char -> TapeMachine -> TapeMachine
changeMachineVal new_val machine =
  let _pos    = pos    machine in
  let _offset = offset machine in
  let _rule   = rule   machine in
  let index   = _pos - _offset in
  let _states = changeAtPos (states machine) index new_val in
  tapeMachine _states _pos _offset _rule

-- simulating the turing machine
selectFunction :: Char -> (TapeMachine -> TapeMachine)
selectFunction symbol
  | symbol == 'L' = moveLeft
  | symbol == 'R' = moveRight
  | otherwise = changeMachineVal symbol

turingMachineStep :: TapeMachine -> TapeMachine
turingMachineStep machine =
  let _pos    = pos    machine in
  let _offset = offset machine in
  let _rule_index = rule machine in
  let rules = filter (\x -> index x == _rule_index) in
  let symbol = states machine !! (_pos - _offset) in
  machine

-- simulateTuringMachine :: TapeMachine -> TapeMachine
-- simulateTuringMachine machine
--   | programTerminated $ rule machine = machine
--   | otherwise =
    -- let _pos    = pos    machine in
    -- let _offset = offset machine in
--     let _rule   = rule   machine in
