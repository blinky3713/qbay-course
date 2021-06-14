module Exercises3 where

import Clash.Prelude
import Test.QuickCheck hiding (resize)

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

{-
-- 2. Number of ones greater or equal to three

numberGEqThreeComb ::
  forall n . (KnownNat n, 1 <= n) =>
  BitVector n -> Bool
numberGEqThreeComb bv = undefined

numberGEqThreeComb_reference :: BitVector 8 -> Bool
numberGEqThreeComb_reference bv = (popCount bv) > 2

prop_numberGEqThreeComb =
  quickCheck (numberGEqThreeComb .==. numberGEqThreeComb_reference)

-- | Takes at most 'n' cycles to determine whether a BitVector
-- has three or more bits set to '1' (i.e. is "bit-serial")
numberGEqThreeSeq ::
  forall n . KnownNat n =>
  Signal System (BitVector n) -> Signal System (Maybe Bool)
numberGEqThreeSeq bvS = undefined

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
