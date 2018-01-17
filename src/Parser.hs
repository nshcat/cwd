
module Parser
    ( parseProgram
    ) where
    
import Control.Concurrent
import Control.Monad
import System.IO
import System.Exit
import Data.Char
import Types
import Utility


-- | Parse given newline-separated string and create program instance
parseProgram :: String -> Program
parseProgram s = map parseLine $ lines s

-- | Parse a whole line of characters and convert them to a list of instructions
parseLine :: String -> [Instruction]
parseLine s = map parseChar s

-- | Convert a single character to its corresponding instruction
parseChar :: Char -> Instruction
parseChar 'o' = Nop
parseChar '+' = IncReg
parseChar '-' = DecReg
parseChar '>' = IncIdx
parseChar '<' = DecIdx
parseChar 'l' = CondTurnLeft
parseChar 'r' = CondTurnRight
parseChar 'L' = TurnLeft
parseChar 'R' = TurnRight
parseChar 'X' = Stop
parseChar 'P' = PrntReg
parseChar 'p' = PrntRegChr
parseChar 'D' = Delay
parseChar ' ' = Stop
parseChar c   = error $ "Invalid character in program: '" ++ [c, '\'']
