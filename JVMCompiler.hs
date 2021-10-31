module JVMCompiler where

-- import Control.Monad.Except
-- import Control.Monad.State
-- import Control.Monad.Except
-- import Control.Monad.State

-- import Data.Text.Internal.Encoding.Fusion (restreamUtf16LE)

import Control.Monad.Except
import Control.Monad.State
import Data.List.NonEmpty (inits)
import Data.Map as Map
import InstantParser.AbsInstant
import InstantParser.LexInstant (Token (Err))
import Text.Parsec (putState)
import Text.Parsec.Prim (putState)

type Env = Map Ident Loc

type StackSize = Integer

type Result = String

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

type Val = (StackSize, Result)

type Loc = Int

initEnv :: Env
initEnv = Map.empty

run :: Program -> IO String
run program = do
  co <- runStateT (runExceptT (compileProgram program)) initEnv
  case fst co of
    (Left e) -> return $ "Error" ++ e
    (Right r) -> return (snd r)

compileProgram :: Program -> Compl Val
compileProgram (Prog stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl Val
compileStmts [] = do return (0, "")
compileStmts (stmt : stmts) = do
  (stackSize, result) <- compileStmt stmt
  (mStackSzie, mResult) <- compileStmts stmts
  return (max stackSize mStackSzie, result ++ mResult)

compileStmt :: Stmt -> Compl Val
compileStmt (SAss ident exp) = do
  (expStackSize, expResult) <- compileExp exp
  env <- get
  let loc = Map.lookup ident env
  newLoc <- case loc of
    Just loc -> return loc
    Nothing -> do
      let newLoc = Map.size env
      let newEnv = Map.insert ident newLoc env
      put newEnv
      return newLoc
  -- TODO: asm result text --
  return (expStackSize, expResult ++ "store " ++ show newLoc ++ "\n")
compileStmt (SExp exp) = do
  (expStackSize, expResult) <- compileExp exp
  --   (printStackSize,printResult) <- printStream
  -- TODO: Print expr --
  return (0, "expr")

printStream :: Compl Val
printStream = do return (0, "")

-- TODO: --
compileExp :: Exp -> Compl Val
compileExp (ExpLit num) = do return (0, "iconst " ++ show num ++ "\n")
compileExp (ExpAdd e1 e2) = compileBinExp e1 e2 "iadd"
compileExp (ExpSub e1 e2) = compileBinExp e1 e2 "isub"
compileExp (ExpMul e1 e2) = compileBinExp e1 e2 "imul"
compileExp (ExpDiv e1 e2) = compileBinExp e1 e2 "idiv"
compileExp (ExpVar ident) = do
  env <- get
  case Map.lookup ident env of
    (Just loc) -> return (0, "iload " ++ show loc ++ "\n")
    Nothing -> throwError "No such ident"

compileBinExp :: Exp -> Exp -> String -> Compl Val
compileBinExp e1 e2 s = do
  (stackSize1, result1) <- compileExp e1
  (stackSize2, result2) <- compileExp e2
  return (max (stackSize1 + 1) stackSize2, result1 ++ result2 ++ "iadd\n")
