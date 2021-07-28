module StackMachine.CoreTypes where

import           Clash.Prelude hiding (Const (..))

type Stack  = Vec 8 Int

data Op     = Add | Mul | Sub | Eq | Neq | Gt | Lt
            deriving (Show, Lift, Eq)

data Instr  = Push Int
            | PushAddr Int
            | PushPC
            | Store Int
            | Calc Op
            | JumpIfZero Int
            | Jump Int
            | EndRep
            | EndProg
            deriving (Show, Lift, Eq)

type Variable = Int

data Expr = Const Int                   -- for constants
          | BinExpr Op Expr Expr        -- for ``binary expressions''
          | Reference Variable
          | IfThenElse Expr Expr Expr
          deriving (Show, Lift, Eq)


data Statement
  = Assign Variable Expr
  | Repeat Expr [Statement]
  deriving (Show,Lift, Eq)


