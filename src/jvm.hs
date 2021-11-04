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
import Text.Parsec.Prim (putState)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStr "<Help>\n"
    [filename] -> do
      code <- readFile filename
      let tokens = myLexer code
      let outputPath = replaceExtension filename ".j"
      let outputDir = takeDirectory outputPath
      let programName = dropExtension $ takeFileName filename

      case pProgram tokens of
        Right program -> do
          result <- run program programName
          case result of
            (Right text) -> do
              writeFile (outputDir ++ "/" ++ programName ++ ".j") text
              processHandle <- runCommand ("java -jar ./lib/jasmin.jar " ++ outputPath ++ " -d " ++ outputDir)
              waitForProcess processHandle
              putStrLn $ "Compiled: " ++ outputPath
            (Left error) -> do
              putStrLn $ "Error:\n" ++ error
        Left error -> do
          putStr ("Error while parsing:\n" ++ error ++ "\n")
      return ()
    _ -> putStr "<Help>\n"