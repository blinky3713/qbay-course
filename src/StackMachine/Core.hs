module StackMachine.Core where

import Clash.Prelude hiding (Const (..))
import qualified Data.List as L

import StackMachine.CoreTypes
import StackMachine.CodeGen

-- ========================================================================
-- Processor functions

xs <~ (i,a) = replace i a xs -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)

type Heap = Vec 256 Int

core
  :: KnownNat n
  => Vec n Instr
  -> (Int,Int, Heap, Stack)
  -> (Int,Int,Heap, Stack)

core instrs (pc,sp,heap, stack) =
  case instrs!!pc of
    Push n   -> (pc', sp+1 , heap, stack <~ (sp,n))

    Calc op  -> (pc', sp-1 , heap, stack <~ (sp-2,v))
             where
               v = alu op (stack!!(sp-2)) (stack!!(sp-1))

    PushAddr n -> (pc', sp + 1, heap, stack <~ (sp, heap !! n))

    Store n -> (pc', sp - 1, heap <~ (n, stack !! sp), stack)

    EndProg  -> (-1, sp, heap, stack)
  where
    pc' = pc+1

-- The program that results in the value of the expression (1105):
prog0 =   Push 2
       :> Push 10
       :> Calc Mul
       :> Push 3
       :> Push 4
       :> Push 11
       :> Calc Add
       :> Calc Mul
       :> Calc Add
       :> Push 12
       :> Push 5
       :> Calc Add
       :> Calc Mul
       :> EndProg
       :> Nil

-- top entity
emptyStack = repeat 0

emptyHeap = repeat 0

prog1 = $(listToVecTH (codeGen expr0))

topEntity
  :: SystemClockResetEnable
  => Signal System (Int, Int, Heap, Stack)
topEntity = s
  where
    s' = core prog0 <$> s
    s  = register (0,0,emptyHeap, emptyStack) s'

-- Testing
test = putStr
     . unlines
     . L.map show
     . takeWhile (\(pc,_,_,_) -> pc /= -1)
     $ sample topEntity
