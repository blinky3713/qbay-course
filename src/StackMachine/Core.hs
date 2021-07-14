module StackMachine.Core where

import           Clash.Prelude          hiding (Const (..))
import qualified Data.List              as L

import           Clash.Prelude.RAM      (asyncRam)
import           Data.Maybe             (fromMaybe)
import           StackMachine.CodeGen
import           StackMachine.CoreTypes

-- ========================================================================
-- Processor functions

xs <~ (i,a) = replace i a xs -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)
                Eq  -> \a b -> if a == b then 1 else 0
                Neq -> \a b -> if a/= b then 1 else 0
                Lt  -> \a b -> if a < b then 1 else 0
                Gt  -> \a b -> if a > b then 1 else 0

type Heap = Vec 256 Int

core
  :: KnownNat n
  => Vec n Instr
  -> Int -- read value
  -> (Int,Int, Stack)
  -> ((Int,Int, Stack), Maybe Int, Maybe (Int, Int)) -- (read addr, (write addr, write val))

core instrs readVal  (pc,sp, stack) =
  case instrs !! pc of
    PushAddr n -> ((pc', sp + 1, stack <~ (sp, readVal)), Just n, Nothing)

    Store n -> ((pc', sp - 1, stack), Nothing, Just (n, stack !! sp))

    EndRep ->
      if stack !! sp - 1 == 0
        then (((stack !! sp - 2) + 1, sp - 2, stack), Nothing, Nothing)
        else let newRepCtr = (stack !! sp - 1) - 1
             in ((stack !! sp - 2, sp, stack <~ (sp - 1, newRepCtr)), Nothing, Nothing)

    PushPC -> ((pc', sp + 1, stack <~ (sp, pc)), Nothing, Nothing)

    JumpIfZero offset ->
      if stack !! sp - 1 == 0
        then ((pc + offset, sp, stack), Nothing, Nothing)
        else ((pc', sp, stack), Nothing, Nothing)

    Jump offset -> ((pc + offset, sp, stack), Nothing, Nothing)

    EndProg  -> ((-1, sp, stack), Nothing, Nothing)

    Push n   -> ((pc', sp+1 , stack <~ (sp,n)), Nothing, Nothing)

    Calc op  -> ((pc', sp-1 , stack <~ (sp-2,v)), Nothing, Nothing)
             where
               v = alu op (stack!!(sp-2)) (stack!!(sp-1))



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


prog1 = $(listToVecTH (codeGen expr0))

topEntity
  :: SystemClockResetEnable
  => Signal System (Int,Int,Stack)
topEntity = s'
  where
    s' :: Signal System (Int,Int,Stack)
    s' = (\(a,_,_) -> a) <$> register ((0,0,emptyStack), Nothing, Nothing) s''
    s'' :: Signal System ((Int, Int, Stack), Maybe Int, Maybe (Int,Int))
    s'' = core prog0 <$> s <*> s'
    s :: Signal System Int
    s  = asyncRam
           (SNat @256)
           ((\(_,mread,_) -> fromMaybe 0 mread) <$> s'')
           ((\(_,_,write) -> write) <$> s'')

-- Testing
test = putStr
     . unlines
     . L.map show
     . takeWhile (\(pc,_,_) -> pc /= -1)
     $ sample topEntity
