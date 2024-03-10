import Data.List (elemIndex)
import Data.Maybe (fromMaybe, Maybe(Just))
import Debug.Trace (trace)

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
  newRule 8  9  '1' 'L',
  newRule 9  9  '1' 'L',
  newRule 9  10 '0' 'L',
  newRule 10 2  '0' 'R',
  newRule 10 10 '1' 'L',
  newRule 11 11 '1' 'L',
  newRule 11 12 '0' 'R']

data TapeMachine = TapeMachine{
  states :: ![Char],
  pos    :: !Int,
  offset :: !Int, -- The offset is the state machine position that corresponds to 0 pos in states list. Allways 0 or less
  rule   :: !Int
} deriving (Show)

tapeMachine :: [Char] -> Int -> Int -> Int -> TapeMachine
tapeMachine states pos offset rule = TapeMachine{ states, pos, offset, rule }

-- Program has terminated if the rule index cant be found
programTerminated :: Int -> Bool
programTerminated dest_index = dest_index `notElem` map index ruleList

newTapeMachine :: [Char] -> TapeMachine
newTapeMachine states = tapeMachine states 0 0 (index $ head ruleList)

-- Where the relative position of the end of states is
end :: TapeMachine -> Int
end machine = length (states machine) + offset machine

-- push functions simulate the list being infinate
beforeOffset, afterEnd :: Int -> TapeMachine -> Bool
beforeOffset new_pos machine = new_pos < offset machine
afterEnd new_pos machine = end machine >= new_pos

-- Highter order push function takes a value to change the offset and a function to determine
-- whether the new position of the tape machine is out of bounds
push :: Int -> ([Char] -> [Char]) -> (TapeMachine -> TapeMachine)
push pos_delta listAppend machine =
  let _states = listAppend $ states machine in
  let _pos    = pos    machine in 
  let _offset = offset machine + pos_delta in
  let _rule   = rule   machine in
  tapeMachine _states _pos _offset _rule 

infix 5 +:
(+:) :: [a] -> a -> [a]
l +: v = l ++ [v]

pushLeft, pushRight :: TapeMachine -> TapeMachine
pushLeft = push (-1) ('0':)
pushRight = push 0 (+:'0')

-- functions for simulating turing machine operations

-- higher order function that takes change in position, a push compare function
-- and a push function and returns a tape machine move function
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
-- to get arround Haskell list imutability a new list is processed with amendment
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

-- function to select the correct rule using input symbol
selectRuleWith :: [Rule] -> Char -> Rule
selectRuleWith rules symbol = rules !! fromMaybe (-1) (symbol `elemIndex` map input rules)

-- function to select the correct tapemachine operation based on the output symbol
selectFunction :: Char -> (TapeMachine -> TapeMachine)
selectFunction 'L' = moveLeft
selectFunction 'R' = moveRight
selectFunction symbol = changeMachineVal symbol

turingMachineStep :: TapeMachine -> (TapeMachine, Int)
turingMachineStep machine = 
  let rule_index = rule machine in
  let rules      = filter (\r -> index r == rule_index) ruleList in
  let symbol     = states machine !! (pos machine - offset machine) in
  let rule       = rules `selectRuleWith` symbol in
  let function   = selectFunction (output rule) in
  (function machine, dest rule)

simulateTuringMachine :: TapeMachine -> TapeMachine-- -> Int -> Int -> TapeMachine
simulateTuringMachine machine -- step max_step
  | let _rule = rule machine in
    let _pos = pos machine in
    let _states = states machine in
    let _offset = offset machine in
    let relative_pos = pos machine - offset machine in
    trace (
      "Applying rule: " ++ show _rule ++ "\n" ++ _states ++ "\n" ++ replicate relative_pos ' ' ++ "^"
    ) False = undefined

simulateTuringMachine machine -- step max_step
  -- | (step == max_step) || programTerminated (rule machine) = machine
  | programTerminated (rule machine) = machine
  | otherwise = 
    let (next_machine, next_rule) = turingMachineStep machine in
    let rule_index = rule next_machine in
    let _states = states next_machine in
    let _pos = pos next_machine in
    let _offset = offset next_machine in
    simulateTuringMachine (tapeMachine _states _pos _offset next_rule) -- (step + 1) max_step

main :: IO()
main =
  let tape_machine = newTapeMachine ['1','1','1','0','0','0'] in
  let result = states $ simulateTuringMachine tape_machine in -- 0 10 in
  print $ "result=" ++ result
