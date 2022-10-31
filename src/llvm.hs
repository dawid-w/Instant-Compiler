module Main where

import InstantParser.AbsInstant
import InstantParser.LexInstant
import InstantParser.ParInstant
import InstantParser.PrintInstant (Print, printTree)
import InstantParser.SkelInstant ()
import LLVMCompiler (run)
import System.Environment (getArgs)
import System.FilePath (dropExtension, replaceExtension, takeDirectory, takeFileName)
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStr helpString
    [filename] -> do
      code <- readFile filename
      let tokens = myLexer code
      let outputPath = replaceExtension filename ".ll"
      let outputDir = takeDirectory outputPath
      let programName = dropExtension $ takeFileName filename

      case pProgram tokens of
        Right program -> do
          result <- run program programName
          case result of
            (Right text) -> do
              writeFile (outputDir ++ "/" ++ programName ++ ".ll") text
              processHandle <- runCommand ("llvm-as " ++ outputPath)
              waitForProcess processHandle
              putStrLn $ "Compiled: " ++ outputPath
            (Left error) -> do
              putStrLn $ "Error:\n" ++ error
        Left error -> do
          putStr ("Error while parsing:\n" ++ error ++ "\n")
      return ()
    _ -> putStr helpString

helpString :: String
helpString = "Usage:     ./insc_llvm <ins file path>\n"