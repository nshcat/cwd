module Main where

import System.IO
import Control.Exception
import System.Environment
import Data.Semigroup ((<>))
import Options.Applicative
import Parser
import Interpreter
import Messaging
import Types


data Configuration = Configuration {
                       filePath    :: String
                     , startPos    :: (Int,Int)
                     , startDir    :: Direction
                     }

parseArguments :: IO(Configuration)
parseArguments = execParser opts
                   where parser = Configuration <$> argument str (metavar "PATH")
                                                <*> option auto
                                                    (long "start-pos"
                                                   <> help "Sets the starting position of the interpreter"
                                                   <> showDefault
                                                   <> value (0,0) )
                                                <*> option auto
                                                    (long "start-dir"
                                                   <> help "Sets the starting direction of the interpreter"
                                                   <> showDefault
                                                   <> value DDown )
                                                                                                                                                   
                         opts   = info (parser <**> helper)
                                   ( fullDesc
                                  <> progDesc "crosswords interpreter"
                                  <> header ("cwd v0.0") )


main :: IO ()
main = do
        cfg <- parseArguments
        catch (action cfg) handler
          where handler = \msg -> putMessage "cwd" $ Message Error $ show (msg::Control.Exception.SomeException);
                action cfg  = do
                               input <- readFile $ filePath cfg
                               let state = makeState (startPos cfg) (startDir cfg)
                               run state $! parseProgram input              

                  
