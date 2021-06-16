module Exercises3 where

import           Clash.Prelude
import           Test.QuickCheck hiding (resize)
import qualified Debug.Trace as Trace

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

-- | Takes at most 'n' cycles to determine whether a BitVector
-- has three or more bits set to '1' (i.e. is "bit-serial")
numberGEqThreeSeq ::
  forall n . KnownNat n =>
  SystemClockResetEnable =>
  Signal System (BitVector n) -> Signal System (Maybe Bool)
numberGEqThreeSeq bsV  =
  let
      x :: Signal System (Int, BitVector n)
      x = let y = f <$> bsV <*> x
          in register (n - 1, 0) y

      w :: Signal System (Maybe Bool)
      w = mealy bitChecker 0 x
  in w
  where
    n = snatToNum (SNat :: SNat n)
    f bs (m, _)
      | m == 0 = (n - 1, bs)
      | otherwise = (m - 1, bs)
    bitChecker :: Int -> (Int, BitVector n) -> (Int, Maybe Bool)
    bitChecker s (m,x)
      | m == 0 =
          let s' = s + fromIntegral (x ! m)
          in (0, Just $ s' >= 3)
      | otherwise =
          let s' = s + fromIntegral (x ! m)
          in (s', Nothing)

-- | Count the number of occurances of a pattern (overlapping matches)
patternOverlapping
  :: forall n k m a .
     KnownNat n
  => KnownNat m
  => Eq a
  => SystemClockResetEnable
  => (k + 1) ~ n
  => NFDataX a
  => Show a
  => Vec n a
  --  The pattern to match against
  -> Signal System a
  -> Signal System (Unsigned m)
patternOverlapping p as =
  let mPattern = map Just p
      f :: Vec n (Maybe a) -> a -> (Vec n (Maybe a), Unsigned m)
      f acc a =
        let acc' :: 1 <= n => Vec n (Maybe a)
            acc' = Just a :> init acc
        in Trace.traceShow acc' $
             if acc' == mPattern then (acc', 1) else (acc', 0)
  in mealy (\s i -> (s + i, s + i)) 0 (mealy f (repeat Nothing) as)




-- | Count the number of occurances of a pattern (non-overlapping matches)
patternNonOverlapping
  :: forall n k m a .
     KnownNat n
  => KnownNat m
  => Eq a
  => SystemClockResetEnable
  => (k + 1) ~ n
  => NFDataX a
  => Show a
  => Vec n a
  --  The pattern to match against
  -> Signal System a
  -> Signal System (Unsigned m)
patternNonOverlapping p as =
  let mPattern = map Just p
      f :: Vec n (Maybe a) -> a -> (Vec n (Maybe a), Unsigned m)
      f acc a =
        let acc' :: 1 <= n => Vec n (Maybe a)
            acc' = Just a :> init acc
        in
          Trace.traceShow acc' $
            if acc' == mPattern then (repeat Nothing, 1) else (acc', 0)
  in mealy (\s i -> (s + i, s + i)) 0 (mealy f (repeat Nothing) as)

foo :: Int -> [Unsigned 4]
foo n =
  sampleN n $ patternNonOverlapping ((1 :: Int) :> 0 :> 1 :> Nil) $ fromList $ cycle [1,0]

bar :: Int -> [Unsigned 4]
bar n =
  sampleN n $ patternOverlapping ((1 :: Int) :> 0 :> 1 :> Nil) $ fromList $ cycle [1,0]

