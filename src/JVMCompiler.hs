module JVMCompiler where

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

run :: Program -> String -> IO (Either Error String)
run program programName = do
  co <- runStateT (runExceptT (compileProgram program)) $ Map.insert (Ident "") 0 initEnv
  let localsLimit = max 1 $ Map.size (snd co)
  case fst co of
    (Left error) -> return $ Left error
    (Right r) ->
      return $
        Right
          ( ".class public " ++ programName ++ "\n"
              ++ ".super java/lang/Object\n"
              ++ "; standard initializer\n"
              ++ ".method public <init>()V\n"
              ++ "    aload_0\n"
              ++ "    invokespecial java/lang/Object/<init>()V\n"
              ++ "    return\n"
              ++ ".end method\n"
              ++ ".method public static main([Ljava/lang/String;)V\n"
              ++ ".limit stack "
              ++ show (fst r)
              ++ "\n"
              ++ ".limit locals "
              ++ show localsLimit
              ++ "\n"
              ++ snd r
              ++ "return\n"
              ++ ".end method\n"
          )

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
  return (expStackSize, expResult ++ genStoreInstr newLoc)
compileStmt (SExp exp) = do
  (expStackSize, expResult) <- compileExp exp
  return (expStackSize + 1, genPrintResult expResult)

genStoreInstr :: Loc -> String
genStoreInstr loc
  | loc >= 0 && loc <= 3 = "istore_" ++ show loc ++ "\n"
  | otherwise = "istore " ++ show loc ++ "\n"

genPrintResult :: String -> String
genPrintResult expResult = "getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++ expResult ++ "invokevirtual java/io/PrintStream/println(I)V\n"

compileExp :: Exp -> Compl Val
compileExp (ExpLit num)
  | num >= 0 && num <= 5 = do return (1, "iconst_" ++ show num ++ "\n")
  | num == -1 = do return (1, "iconst_m1" ++ "\n")
  | num >= -128 && num < 128 = do return (1, "bipush " ++ show num ++ "\n")
  | num >= -32768 && num < 32768 = do return (1, "sipush " ++ show num ++ "\n")
  | otherwise = do return (1, "ldc " ++ show num ++ "\n")
compileExp (ExpAdd e1 e2) = compileBinExp e1 e2 "iadd"
compileExp (ExpSub e1 e2) = compileBinExp e1 e2 "isub"
compileExp (ExpMul e1 e2) = compileBinExp e1 e2 "imul"
compileExp (ExpDiv e1 e2) = compileBinExp e1 e2 "idiv"
compileExp (ExpVar ident) = do
  env <- get
  case Map.lookup ident env of
    (Just loc) -> return (1, genLoadInstr loc)
    Nothing -> do
      let (Ident name) = ident
      throwError $ "Unknown ident: " ++ name

genLoadInstr :: Loc -> String
genLoadInstr loc
  | loc >= 0 && loc <= 3 = "iload_" ++ show loc ++ "\n"
  | otherwise = "iload " ++ show loc ++ "\n"

compileBinExp :: Exp -> Exp -> String -> Compl Val
compileBinExp e1 e2 s = do
  (stackSize1, result1) <- compileExp e1
  (stackSize2, result2) <- compileExp e2
  -- Wykonuje pierwsza, zostawiam wynik (+1) wykonuje druga
  return (max stackSize1 (stackSize2 + 1), result1 ++ result2 ++ s ++ "\n")
