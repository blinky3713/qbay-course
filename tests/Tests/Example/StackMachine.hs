module Tests.Example.StackMachine where

import StackMachine.Core
import StackMachine.CoreTypes
import StackMchine.CodeGen


import Test.Hspec
import Test.QuickCheck

stackMachineSpec :: IO ()
stackMachineSpec = hspec $ do
  describe "Can codegen Expressions" $ do
    it "Can codegen binary expression" $ do
      let binExpr = BinExpr Add (Const 1) (Const 2)
          code = [Push 1, Push 2, Op Add, EndProg]
      codeGen binExpr `shouldBe` code
