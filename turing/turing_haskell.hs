data Link = Link {
  source      :: !Int,
  destination :: !Int,
  value1      :: !Char,
  value2      :: !Char
} deriving Show

data StateMachine = StateMachine {
  links   :: ![Link],
  states  :: ![Char],
  tapePos :: !Int
} deriving Show

link :: Int -> Int -> Char -> Char -> Link
link source destination value1 value2 = Link{source, destination, value1, value2}

maxStateAddress :: [Link] -> Int
maxStateAddress links = maximum(map source links)

initStates :: [Link] -> [Char]
initStates links = replicate (maxStateAddress links) '0'

stateMachine :: [Link]  -> StateMachine
stateMachine links = StateMachine{links, states = initStates links, tapePos = 1}

sourceLinks :: [Link] -> Int -> [Link]
sourceLinks links address = filter (\x -> source x == address) links

currentLinks :: StateMachine -> [Link]
currentLinks machine = sourceLinks (links machine) (tapePos machine)

currentState :: StateMachine -> Char
currentState machine = states machine !! (tapePos machine - 1)

nextState :: [Link] -> Char -> Char
nextState =

simulateStateMachine :: StateMachine -> [Char]
simulateStateMachine machine =
  let current_links = currentLinks machine in
  let state = currentState machine in
    states machine

main :: IO()
main = do

  let state_machine = stateMachine [
        link 1  2  '1' 'L',
        link 2  3  '0' 'L',
        link 2  3  '1' 'L',
        link 3  3  '0' '1',
        link 3  4  '1' 'L',
        link 4  4  '0' '1',
        link 4  5  '1' 'R',
        link 5  5  '1' 'R',
        link 5  6  '0' 'R',
        link 6  6  '1' 'R',
        link 6  7  '0' 'L',
        link 7  7  '1' '0',
        link 7  8  '0' 'L',
        link 8  11 '0' 'L',
        link 11 11 '1' 'L',
        link 11 12 '0' 'R',
        link 8  9  '1' 'L',
        link 9  9  '1' 'L',
        link 9  10 '0' 'L',
        link 10 2  '0' 'R'
        ] in
      print state_machine
