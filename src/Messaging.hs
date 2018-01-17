module Messaging
    ( putMessage
    , Message(..)
    , MessageType(..)
    ) where
    
    
import System.IO
import System.Console.ANSI


data MessageType = Error
                 | Warning
                 | Note
                 | Remark
                 | Info
                 deriving (Eq, Show)

data Message = Message MessageType String
             deriving (Eq, Show)

-- | Display given message instance on the command line.
--   If the first argument is empty, no source string is displayed.
putMessage :: String -> Message -> IO()
putMessage s (Message t m) = do
                              putSource s
                              putMessageType t    
                              putStrLn m
                  

putSource :: String -> IO()
putSource [] = return ()
putSource s  = putStr s >> putStr ": "

putMessageType :: MessageType -> IO()
putMessageType t = do
                    setSGR [SetColor Foreground Vivid $ typeColor t]
                    putStr $ typeString t
                    setSGR [Reset]
                    putStr ": "
                   
typeColor :: MessageType -> Color
typeColor Error   = Red
typeColor Warning = Yellow
typeColor Remark  = Black
typeColor Note    = White
typeColor Info    = White
                  
typeString :: MessageType -> String
typeString Error   = "error"
typeString Warning = "warning"
typeString Remark  = "remark"
typeString Note    = "note"
typeString Info    = "info"
             
                  

                 


