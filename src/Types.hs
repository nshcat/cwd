module Types
    ( Program
    , Instruction(..)
    , Direction(..)
    ) where


type Program = [[Instruction]] -- TODO: Replace with 2D array

data Direction = DLeft
               | DRight
               | DUp
               | DDown
               deriving (Eq)
               
               
instance Read Direction where
  readsPrec _ "Left"  = [(DLeft, "")]
  readsPrec _ "Right" = [(DRight, "")]
  readsPrec _ "Up"    = [(DUp, "")]
  readsPrec _ "Down"  = [(DDown, "")]
  readsPrec _ s  = error $ "Can't read direction \"" ++ s ++ "\""
  
instance Show Direction where
  show DLeft = "Left"
  show DRight = "Right"
  show DUp = "Up"
  show DDown = "Down"

-- TODO: Change Console Color etc
data Instruction = IncReg         -- ^ Increment register at current index
                 | DecReg         -- ^ Decrement register at current index
                 | PrntRegChr     -- ^ Print contents of register at current index, interpreted as a character
                 | PrntReg        -- ^ Print contents of register at current index, interpreted as an integer
                 | IncIdx         -- ^ Increment register index
                 | DecIdx         -- ^ Decrement register index
                 | CondTurnLeft   -- ^ Turn left if register at current index is 0
                 | TurnLeft       -- ^ Unconditionally turn left
                 | CondTurnRight  -- ^ Turn right if register at current index is 0
                 | TurnRight      -- ^ Unconditionally turn right
                 | Nop            -- ^ No operation
                 | Stop           -- ^ Stop execution. This is most often issued on out-of-bounds run
                 | Delay          -- ^ Delay execution by a fixed amount of time
                 deriving (Eq, Show)
                 


