module Main where

import InstantParser.AbsInstant
import InstantParser.LexInstant
import InstantParser.ParInstant
import JVMCompiler (run)
import InstantParser.PrintInstant (Print, printTree)
import InstantParser.SkelInstant ()
import System.Environment (getArgs)
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStr "<Help>\n"
    [filename] -> do
      code <- readFile filename
      let tokens = myLexer code

      case pProgram tokens of
        Right program -> do
          putStr "Compiling..."
          let result = run program
          processHandle <- runCommand "jasmin command"
          waitForProcess processHandle
          putStr result
        Left err -> do
          putStr ("Error while parsing:" ++ err)

      return ()
    _ -> putStr "<Help>\n"