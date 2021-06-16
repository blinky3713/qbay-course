module Exercises3 where

import Clash.Prelude
import Test.QuickCheck hiding (resize)
import Data.Bifunctor (second)
import Data.Maybe (isJust)
import Debug.Trace as Trace

-- 1. Moving avarage

-- Use the 'register' function, and the 'liftA' family of functions
-- ('liftA', 'liftA2', 'liftA3')
movingAvgApp ::
  forall n . (KnownNat n, SystemClockResetEnable) =>
  Signal System (Signed n) -> Signal System (Signed n)
movingAvgApp xs =
    let acc = register ((0,0), 0) accN
        accN = f <$> acc <*> xs
    in fst . fst <$> acc
  where
    f ((accQ,accR), n) x = (quotRem (accQ * n + accR + x) (n + 1), n + 1)


-- Use 'mealy' or 'moore'
movingAvgCombinator ::
  forall n . (KnownNat n, SystemClockResetEnable) =>
  Signal System (Signed n) -> Signal System (Signed n)
movingAvgCombinator = mealy f ((0,0), 0)
  where
    f ((accQ,accR), n) x =
      let a = (quotRem (accQ * n + accR + x) (n + 1), n + 1)
      in (a, fst . fst $ a)


-- 2. Number of ones greater or equal to three

numberGEqThreeComb ::
  forall n . (KnownNat n, 1 <= n) =>
  BitVector n -> Bool
numberGEqThreeComb bv =
  let s = sum . map f . bv2v $ bv
  in s >= 3
  where
    f b = if b == high then (1 :: Int) else 0

numberGEqThreeComb_reference :: BitVector 8 -> Bool
numberGEqThreeComb_reference bv = popCount bv > 2

prop_numberGEqThreeComb =
  quickCheck (numberGEqThreeComb .==. numberGEqThreeComb_reference)

{-

   z --- x -- y --- w
   | <--- - -- - -- |

-}

-- | Takes at most 'n' cycles to determine whether a BitVector
-- has three or more bits set to '1' (i.e. is "bit-serial")
numberGEqThreeSeq ::
  forall n . KnownNat n =>
  SystemClockResetEnable =>
  Signal System (BitVector n) -> Signal System (Maybe Bool)
numberGEqThreeSeq bsV  =
  let n = snatToNum (SNat :: SNat n)
      x :: Signal System (Int, BitVector n)
      x = let y = f <$> bsV <*> x
          in register (n - 1, 0) y

      w :: Signal System (Maybe Bool)
      w = mealy bitChecker 0 x
  in w
  where
    f bs (m, _) = (m - 1, bs)
    bitChecker :: Int -> (Int, BitVector n) -> (Int, Maybe Bool)
    bitChecker s (m,x)
      | m == 0 =
          let s' = s + fromIntegral (x ! m)
          in (0, Just $ s' >= 3)
      | otherwise =
          let s' = s + fromIntegral (x ! m)
          in if s' >= 3 then (0, Just True) else (s', Nothing)





{-
-- 3. Pattern recognizer

-- | Count the number of occurances of a pattern (overlapping matches)
patternOverlapping ::
  forall n m a . (KnownNat n, Eq a, SystemClockResetEnable) =>
  -- | The pattern to match against
  Vec n a ->
  Signal System a -> Signal System (Unsigned m)
patternOverlapping = undefined

-- | Count the number of occurances of a pattern (non-overlapping matches)
patternNonOverlapping ::
  forall n m a . (KnownNat n, Eq a, SystemClockResetEnable) =>
  -- | The pattern to match against
  Vec n a ->
  Signal System a -> Signal System (Unsigned m)
patternNonOverlapping = undefined
-}
