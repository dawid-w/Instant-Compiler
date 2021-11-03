module LLVMCompiler where

import Control.Monad.Except
import Control.Monad.State
import Data.List.NonEmpty (inits)
import Data.Map as Map
import InstantParser.AbsInstant
import InstantParser.LexInstant (Token (Err))
import Text.Parsec (putState)
import Text.Parsec.Prim (putState)

type Env = (Map Ident Loc, Int)

type StackSize = Int

type Result = String

type Error = String

type Compl a = ExceptT Error (StateT Env IO) a

type RegNum = Int

type Val = (RegNum, Result)

type Loc = Int

initEnv :: Env
initEnv = (Map.empty, 0)

run :: Program -> String -> IO String
run program programName = do
  co <- runStateT (runExceptT (compileProgram program)) initEnv
  case fst co of
    (Left e) -> return $ "Error" ++ e
    (Right r) ->
      return
        ( "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n"
            ++ "declare i32 @printf(i8*, ...)\n"
            ++ "define void @printInt(i32 %x) {\n"
            ++ "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n"
            ++ "call i32 (i8*, ...) @printf(i8* %t0, i32 %x) \n"
            ++ "ret void\n"
            ++ "}\n"
            ++ "define i32 @main() {\n"
            ++ snd r
            ++ "ret i32 0\n"
            ++ "}\n"
        )

compileProgram :: Program -> Compl Val
compileProgram (Prog stmts) = compileStmts stmts

compileStmts :: [Stmt] -> Compl Val
compileStmts [] = do return (0, "")
compileStmts (stmt : stmts) = do
  (_, result) <- compileStmt stmt
  (_, mResult) <- compileStmts stmts
  return (-1, result ++ mResult)

compileStmt :: Stmt -> Compl Val
compileStmt (SAss ident exp) = do
  (resultR, expResult) <- compileExp exp
  (map, nextR) <- get
  let newMap = Map.insert ident nextR map
  put (newMap, nextR + 1)
  return (nextR, expResult ++ "%v" ++ show nextR ++ " = add i32 0, %v" ++ show resultR ++ "\n")
compileStmt (SExp exp) = do
  (resultR, expResult) <- compileExp exp
  (map, nextR) <- get
  put (map, nextR + 1)
  return (nextR, expResult ++ "%v" ++ show nextR ++ " = add i32 0, %v" ++ show resultR ++ "\n" ++ "call void @printInt(i32 %v" ++ show nextR ++ ")\n")

compileExp :: Exp -> Compl Val
compileExp (ExpLit num) = do
  (map, nextR) <- get
  put (map, nextR + 1)
  return (nextR, "%v" ++ show nextR ++ " = " ++ "add" ++ " i32 " ++ "0" ++ ", " ++ show num ++ "\n")
compileExp (ExpAdd e1 e2) = compileBinExp e1 e2 "add"
compileExp (ExpSub e1 e2) = compileBinExp e1 e2 "sub"
compileExp (ExpMul e1 e2) = compileBinExp e1 e2 "mul"
compileExp (ExpDiv e1 e2) = compileBinExp e1 e2 "div"
compileExp (ExpVar ident) = do
  (map, nextR) <- get
  put (map, nextR + 1)
  let (Ident name) = ident
  case Map.lookup ident map of
    (Just loc) -> return (nextR, "%v" ++ show nextR ++ " = add i32 0, %v" ++ show loc ++ "\n")
    Nothing -> throwError "No such ident"

compileBinExp :: Exp -> Exp -> String -> Compl Val
compileBinExp e1 e2 s = do
  (reg1, result1) <- compileExp e1
  (reg2, result2) <- compileExp e2
  (map, nextR) <- get
  put (map, nextR + 1)
  return (nextR, result1 ++ result2 ++ "%v" ++ show nextR ++ " = " ++ s ++ " i32 %v" ++ show reg1 ++ ", %v" ++ show reg2 ++ "\n")
