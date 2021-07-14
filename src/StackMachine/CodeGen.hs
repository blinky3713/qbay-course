module StackMachine.CodeGen where

import           Clash.Prelude          hiding (Const (..))
import           Control.Monad.State
import qualified Data.List              as L

import           StackMachine.CoreTypes

-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr0 = BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))

class CodeGen a where
  codeGen :: a -> [Instr]

instance CodeGen Expr where
  codeGen _expr = ( <> [EndProg]) $ codeGen' _expr
    where
      codeGen' expr = case expr of
        Const n -> [Push n]
        binExpr@BinExpr{} ->
          let
            go :: [Instr] -> Expr -> [Instr]
            go acc (BinExpr op a b) =
              let accA = go [] a
                  accB = go [] b
              in acc <> accA <> accB <> [Calc op]
            go acc e = acc <> codeGen' e
          in go [] binExpr
        Reference n -> [PushAddr n]
        IfThenElse b _then _else ->
          let bc = codeGen' b
              tc = codeGen' _then
              ec = codeGen' _else
          in bc <> [JumpIfZero (L.length tc + 1)] <> (tc <> [Jump (L.length ec + 1)]) <> ec


instance CodeGen Statement where
  codeGen (Assign n expr) =
    PushAddr n : codeGen expr
  codeGen (Repeat expr stmts) =
      L.concatMap codeGen stmts <> [EndRep] <> codeGen expr <> [PushPC]

