{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreter
    ( run
    , initialState
    , makeState
    ) where
    
import Control.Concurrent
import Control.Monad
import System.IO
import System.Exit
import Data.Char
import Types
import Utility


type Position = (Int, Int)
type Register = Int
type RegisterFile = [Register] -- Replace with array later.

data Turn = TLeft
          | TRight
          deriving (Show, Eq)
          
data State = State
    { regIndex  :: Int           -- ^ Current register file index
    , direction :: Direction     -- ^ Current interpretation direction
    , position  :: Position      -- ^ Current position in the program
    , registers :: RegisterFile  -- ^ Register file
    } deriving (Eq, Show)


-- | The number of registers available to the interpreted program
registerCount :: Int
registerCount = 10


-- | Creates an initialized state instance
initialState :: State
initialState = State 0 DDown (0,0) $ replicate registerCount 0

-- | Create an initial state configuration with given start position and direction
makeState :: Position -> Direction -> State
makeState p d = initialState{position=p, direction=d}

-- | The amount of time slept when delay instruction is encountered (in microseconds)
delayAmount :: Int
delayAmount = 100000


-- | Retrieves instruction at given position
instructionAt :: Program -> Position -> Instruction
instructionAt p (x,y) | y < 0 || x < 0 || y >= h || x >= w = Stop
                      | otherwise = (p !! y) !! x
    where h = length p;
          w = length $ p !! y
          
-- | Update register that is currently selected by the index with given modifier.
--   The modifier will be added to its value.
updateRegister :: Int -> State -> State
updateRegister mod s@State{..} = s{registers = regs'}
    where regs'  = replaceElement registers regIndex newval;
          newval = (+) mod $ registers !! regIndex
          
-- | Determines new interpretation direction based on turn direction 
turn :: Direction -> Turn -> Direction
turn DUp TLeft     = DLeft
turn DUp TRight    = DRight
turn DDown TLeft   = DRight
turn DDown TRight  = DLeft
turn DLeft TRight  = DUp
turn DLeft TLeft   = DDown
turn DRight TRight = DDown
turn DRight TLeft  = DUp


-- | Computes the delta that has to be applied to the position in order
--   to make a move in given direction
directionDelta :: Direction -> (Int, Int)
directionDelta DUp    = (0,-1)
directionDelta DDown  = (0,1)
directionDelta DLeft  = (-1,0)
directionDelta DRight = (1,0)

-- | Performs turn only when boolean flag evaluates to true
condTurn :: Direction -> Turn -> Bool -> Direction
condTurn d t True = turn d t
condTurn d _ _    = d

-- | Modify current position according to currently set direction
performMove :: State -> State
performMove s@State{..} = s{position = (x',y')}
   where delta = directionDelta direction;
         x'    = (+) x $ fst delta;
         y'    = (+) y $ snd delta;
         x     = fst position;
         y     = snd position


-- | Interpret a single instruction and update state accordingly. This will perform IO
-- if requested by the instruction.
interpretInstr :: State -> Instruction -> IO(State)
interpretInstr s Nop                     = return s
interpretInstr s Delay                   = threadDelay delayAmount >> return s
interpretInstr _ Stop                    = putStrLn "End of program reached" >> exitSuccess
interpretInstr s IncReg                  = return $ updateRegister (1) s
interpretInstr s DecReg                  = return $ updateRegister (-1) s
interpretInstr s@State{..} IncIdx        = return $ s{regIndex = regIndex + 1}
interpretInstr s@State{..} DecIdx        = return $ s{regIndex = regIndex - 1}
interpretInstr s@State{..} PrntRegChr    = putStrLn [chr $ registers !! regIndex] >> return s
interpretInstr s@State{..} PrntReg       = (putStrLn $ show $ registers !! regIndex) >> return s    
interpretInstr s@State{..} TurnLeft      = return $ s{direction = turn direction TLeft}
interpretInstr s@State{..} TurnRight     = return $ s{direction = turn direction TRight} 
interpretInstr s@State{..} CondTurnLeft  = return $ s{direction = condTurn direction TLeft $ (==) 0 $ registers !! regIndex}
interpretInstr s@State{..} CondTurnRight = return $ s{direction = condTurn direction TRight $ (==) 0 $ registers !! regIndex}


-- | Perform a single interpreter tick, which means performing a move and interpreting one instruction
performTick :: State -> Program -> IO(State)
performTick s p = interpretInstr s' i
                    where s' = performMove s;
                          i  = instructionAt p $ position s  -- We use the old value here, since thats whats done in the original cwd interpreter.
                   


-- | High level interpreter function. Will run given program until termination.
run :: State -> Program -> IO()
run s p = foldM (\a _ -> performTick a p) s [1..] >> return ()


