module Main where

import InstantParser.AbsInstant
import InstantParser.LexInstant
import InstantParser.ParInstant
import InstantParser.PrintInstant (Print, printTree)
import InstantParser.SkelInstant ()
import JVMCompiler (run)
import System.Environment (getArgs)
import System.FilePath (dropExtension, replaceExtension, takeDirectory, takeFileName)
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStr "<Help>\n"
    [filename] -> do
      code <- readFile filename
      let tokens = myLexer code
      let outputPath = replaceExtension filename ".j"
      let programName = dropExtension $ takeFileName filename

      case pProgram tokens of
        Right program -> do
          result <- run program programName

          writeFile (programName ++ ".j") result
          processHandle <- runCommand ("java -jar ./lib/jasmin-2.4/jasmin.jar " ++ programName ++ ".j")
          waitForProcess processHandle
          putStrLn "Compiled\n"
        Left err -> do
          putStr ("Error while parsing:" ++ err)

      return ()
    _ -> putStr "<Help>\n"