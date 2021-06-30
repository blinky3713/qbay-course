module Matrix where

import Clash.Prelude

dot
  :: Num a
  => 1 <= n
  => KnownNat n
  => Vec n a
  -> Vec n a
  -> a
dot a b = sum $ zipWith (*) a b


vadd
  :: KnownNat n
  => Num a
  => Vec n a
  -> Vec n a
  -> Vec n a
vadd (Cons a as) (Cons b bs) = a + b :> vadd as bs
vadd _ _ = repeat 0

type Matrix m n a = Vec m (Vec n a)

madd
  :: Num a
  => KnownNat m
  => KnownNat n
  => Matrix m n a
  -> Matrix m n a
  -> Matrix m n a
madd = zipWith vadd

mulMV
  :: KnownNat m
  => KnownNat n
  => 1 <= n
  => Num a
  => Matrix m n a
  -> Vec n a
  -> Vec m a
mulMV m v = map (dot v) m

mmul
  :: forall a m n l.
     Num a
  => KnownNat m
  => KnownNat n
  => KnownNat l
  => 1 <= m
  => 1 <= n
  => 1 <= l
  => Matrix m n a
  -> Matrix n l a
  -> Matrix m l a
mmul ml mr =
  map (mulMV (transpose mr)) ml
