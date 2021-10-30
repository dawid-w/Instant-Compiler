module JVMCompiler where

import Data.Map as Map (Map)
import InstantParser.AbsInstant (Ident, Program)

type Env = Map Ident Loc

type Loc = Int

run :: Program -> String
run filename = "Compiled program"
