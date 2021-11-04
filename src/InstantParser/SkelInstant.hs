-- File generated by the BNF Converter (bnfc 2.9.3).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module InstantParser.SkelInstant where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified InstantParser.AbsInstant

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: InstantParser.AbsInstant.Ident -> Result
transIdent x = case x of
  InstantParser.AbsInstant.Ident string -> failure x

transProgram :: InstantParser.AbsInstant.Program -> Result
transProgram x = case x of
  InstantParser.AbsInstant.Prog stmts -> failure x

transStmt :: InstantParser.AbsInstant.Stmt -> Result
transStmt x = case x of
  InstantParser.AbsInstant.SAss ident exp -> failure x
  InstantParser.AbsInstant.SExp exp -> failure x

transExp :: InstantParser.AbsInstant.Exp -> Result
transExp x = case x of
  InstantParser.AbsInstant.ExpAdd exp1 exp2 -> failure x
  InstantParser.AbsInstant.ExpSub exp1 exp2 -> failure x
  InstantParser.AbsInstant.ExpMul exp1 exp2 -> failure x
  InstantParser.AbsInstant.ExpDiv exp1 exp2 -> failure x
  InstantParser.AbsInstant.ExpLit integer -> failure x
  InstantParser.AbsInstant.ExpVar ident -> failure x
