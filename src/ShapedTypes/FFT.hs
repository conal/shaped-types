{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ParallelListComp       #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}

-- experiment
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE UndecidableInstances #-} -- See below

{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP

#define TESTING

#ifdef TESTING
{-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP
#endif

-- #define GenericPowFFT

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.FFT
-- Copyright   :  (c) 2015-2016 Conal Elliott
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Generic FFT
----------------------------------------------------------------------

module ShapedTypes.FFT
  ( FFT(..), DFTTy, genericFft, dftT, GFFT
  -- -- Temporary while debugging
  -- , twiddle, twiddles, omega, cis
  ) where

import Prelude hiding (zipWith)
import GHC.Generics hiding (C,S)

#ifdef TESTING
import Data.Foldable (toList)
-- import Test.QuickCheck (quickCheck)
import Test.QuickCheck.All (quickCheckAll)
import ShapedTypes.ApproxEq
#endif

import Data.Key

import Circat.Misc (transpose,Unop)
import Circat.Complex

import ShapedTypes.Misc
import ShapedTypes.Sized
import ShapedTypes.Scan (LScan,LFScan,lproducts,iota) -- , lsums
#ifndef GenericPowFFT
import ShapedTypes.Nat
#endif
import ShapedTypes.Pair
import ShapedTypes.Vec
import ShapedTypes.LPow (LPow)
import ShapedTypes.RPow (RPow)
import qualified ShapedTypes.LPow as L
import qualified ShapedTypes.RPow as R

{--------------------------------------------------------------------
    FFT
--------------------------------------------------------------------}

type DFTTy f = forall a. RealFloat a => f (Complex a) -> FFO f (Complex a)

class FFT f where
  type FFO f :: * -> *
  fft :: DFTTy f
  -- Temporary hack to avoid newtype-like representation.
  fftDummy :: f a
  fftDummy = undefined

type AFS h = (Applicative h, Zip h, Foldable h, Sized h, LScan h)

twiddle :: forall g f a. (AFS g, AFS f, RealFloat a) => Unop (g (f (Complex a)))
twiddle = (zipWith.zipWith) (*) twiddles
{-# INLINE twiddle #-}

twiddles :: forall g f a. (AFS g, AFS f, RealFloat a) => g (f (Complex a))
twiddles = powers <$> powers (omega (size @(g :.: f)))

omega :: (Integral n, RealFloat a) => n -> Complex a
omega n = cis (- 2 * pi / fromIntegral n)
-- omega n = exp (0 :+ (- 2 * pi / fromIntegral n))
-- omega n = exp (- 2 * (0:+1) * pi / fromIntegral n)
{-# INLINE omega #-}

-- | @'exp' (i * a)
cis :: RealFloat a => a -> Complex a
cis a = cos a :+ sin a

-- Powers of x, starting x^0. Uses 'LScan' for log parallel time
powers :: (LFScan f, Applicative f, Num a) => a -> f a
powers = fst . lproducts . pure
{-# INLINE powers #-}

-- TODO: Consolidate with powers in TreeTest and rename sensibly. Maybe use
-- "In" and "Ex" suffixes to distinguish inclusive and exclusive cases.

{--------------------------------------------------------------------
    Generic support
--------------------------------------------------------------------}

instance FFT Par1 where
  type FFO Par1 = Par1
  fft = id

instance ( Applicative f , Zip f,  Traversable f , Traversable g
         , Applicative (FFO f), Zip (FFO g), Applicative (FFO g), Traversable (FFO g)
         , FFT f, FFT g, LScan f, LScan (FFO g), Sized f, Sized (FFO g) )
      => FFT (g :.: f) where
  type FFO (g :.: f) = FFO f :.: FFO g
  fft = inComp (traverse fft . twiddle . traverse fft . transpose)
  {-# INLINE fft #-}

#if 0
  fft = Comp1 . transpose . fmap fft . twiddle . transpose . fmap fft . transpose . unComp1
  fft = Comp1 . traverse fft . twiddle . traverse fft . transpose . unComp1

-- Types in fft for (g :. f):
  unComp1   :: (g :. f) a -> g  (f  a)
  transpose :: g  (f  a)  -> f  (g  a)
  fmap fft  :: f  (g  a)  -> f  (g' a)
  transpose :: f  (g' a)  -> g' (f  a)
  twiddle   :: g' (f  a)  -> g' (f  a)
  fmap fft  :: g' (f  a)  -> g' (f' a)
  transpose :: g' (f' a)  -> f' (g' a)
  Comp1     :: g  (f a)   -> (g :. f) a
#endif

-- | Generic FFT
genericFft :: ( Generic1 f, Generic1 (FFO f)
              , FFT (Rep1 f), FFO (Rep1 f) ~ Rep1 (FFO f) ) => DFTTy f
genericFft = inGeneric1 fft

-- | Generic FFT

-- genericFft :: DFTTy f

-- genericFft :: ( FFO (Rep1 f) ~ Rep1 g, FFT (Rep1 f)
--               , Generic1 g, Generic1 f, RealFloat a )
--            => f (Complex a) -> g (Complex a)
-- genericFft = inGeneric1 fft

type GFFT f = (Generic1 f, Generic1 (FFO f), FFT (Rep1 f), FFO (Rep1 f) ~ Rep1 (FFO f))

#define GenericFFT(f,g) instance GFFT (f)(g) => FFT (f)(g) where fft = genericFft

-- Generalization of 'dft' to traversables.
dftT :: forall f a. (AFS f, Traversable f, RealFloat a)
     => Unop (f (Complex a))
dftT xs = out <$> indices
 where
   out k   = sum (zipWith (\ n x -> x * ok^n) indices xs) where ok = om ^ k
   indices = iota :: f Int
   om      = omega (size @f)
{-# INLINE dftT #-}

-- TODO: Replace Applicative with Zippable

-- Perhaps dftT isn't very useful. Its result and argument types match, unlike fft.

{--------------------------------------------------------------------
    Specialized FFT instances.
--------------------------------------------------------------------}

-- I put the specific instances here in order to avoid an import loop between
-- the LPow and RPow modules. I'd still like to find an elegant FFT that maps f
-- to f, and then move the instances to RPow and LPow.

-- Radix 2 butterfly
instance FFT Pair where
  type FFO Pair = Pair
  fft (a :# b) = (a + b) :# (a - b)
  {-# INLINE fft #-}

instance ( Applicative (Vec n), Zip (Vec n), Traversable (Vec n), Sized (Vec n) )
      => FFT (Vec n) where
  type FFO (Vec n) = Vec n
  fft = dftT
  {-# INLINE fft #-}

#ifdef GenericPowFFT

GenericFFT(RPow h n, LPow k n)
GenericFFT(LPow h n, RPow k n)

#else

type ATS f = (Applicative f, Zip f, Traversable f, Sized f)

instance FFT (RPow h Z) where
  type FFO (RPow h Z) = (LPow h Z)
  fft = L.L . R.unL
  {-# INLINE fft #-}

instance ( ATS h, ATS (FFO h), ATS (LPow (FFO h) n), ATS (RPow h n)
         , LScan (RPow h n), LScan (FFO h)
         , FFT h, FFT (RPow h n), FFO (RPow h n) ~ LPow (FFO h) n )
      => FFT (RPow h (S n)) where
  type FFO (RPow h (S n)) = LPow (FFO h) (S n)
  fft = L.B . unComp1 . fft . Comp1 . R.unB
  {-# INLINE fft #-}

instance FFT (LPow h Z) where
  type FFO (LPow h Z) = RPow h Z
  fft = R.L . L.unL
  {-# INLINE fft #-}

instance ( ATS h, ATS (FFO h), ATS (RPow (FFO h) n), ATS (LPow h n)
         , LScan (RPow (FFO h) n), LScan h
         , FFT h, FFT (LPow h n), FFO (LPow h n) ~ RPow (FFO h) n )
      => FFT (LPow h (S n)) where
  type FFO (LPow h (S n)) = RPow (FFO h) (S n)
  fft = R.B . unComp1 . fft . Comp1 . L.unB
  {-# INLINE fft #-}

-- TODO: Revisit these constraints, which don't quite have the duality I expected.
#endif

#ifdef TESTING

dft :: forall f a. (AFS f, RealFloat a) => Unop (f (Complex a))
dft as = (<.> as) <$> twiddles
{-# INLINE dft #-}

#if 0
twiddles :: (AFS g, AFS f, RealFloat a) => g (f (Complex a))

as :: f C
(<.> as) :: f C -> C
twiddles :: f (f C)
(<.> as) <$> twiddles :: f C
#endif

-- Binary dot product
infixl 7 <.>
(<.>) :: (Foldable f, Zip f, Num a) => f a -> f a -> a
u <.> v = sum (zipWith (*) u v)

{--------------------------------------------------------------------
    Simple, quadratic DFT (for specification & testing)
--------------------------------------------------------------------}

-- Adapted from Dave's definition
dftL :: RealFloat a => Unop [Complex a]
dftL xs = [ sum [ x * ok^n | x <- xs | n <- [0 :: Int ..] ]
          | k <- [0 .. length xs - 1], let ok = om ^ k ]
 where
   om = omega (length xs)

{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

-- > powers 2 :: LTree N2 Int
-- B (B (L ((1 :# 2) :# (4 :# 8))))
-- > powers 2 :: LTree N3 Int
-- B (B (B (L (((1 :# 2) :# (4 :# 8)) :# ((16 :# 32) :# (64 :# 128))))))

-- PrettyDouble doesn't yet have an Arbitrary instance, so use Double for now
type C = Complex Double

fftl :: (FFT f, Foldable (FFO f), RealFloat a) => f (Complex a) -> [Complex a]
fftl = toList . fft

type LTree = L.Pow Pair
type RTree = R.Pow Pair

type LC n = LTree n C
type RC n = RTree n C

p1 :: Pair C
p1 = 1 :# 0

tw1 :: LTree N1 (Pair C)
tw1 = twiddles

tw2 :: LTree N2 (Pair C)
tw2 = twiddles

-- Adapted from Dave's testing

-- test :: (FFT f, Foldable f, Foldable (FFO f)) => f C -> IO ()
-- test fx =
--   do ps "\nTesting input" xs
--      ps "Expected output" (dftL xs)
--      ps "Actual output  " (toList (fft fx))
--  where
--    ps label z = putStrLn (label ++ ": " ++ show z)
--    xs = toList fx

#if 0
t0 :: LC N0
t0 = L.fromList [1]

t1 :: LC N1
t1 = L.fromList [1, 0]

t2s :: [LC N2]
t2s = L.fromList <$>
        [ [1,  0,  0,  0]  -- Delta
        , [1,  1,  1,  1]  -- Constant
        , [1, -1,  1, -1]  -- Nyquist
        , [1,  0, -1,  0]  -- Fundamental
        , [0,  1,  0, -1]  -- Fundamental w/ 90-deg. phase lag
       ]

tests :: IO ()
tests = do test p1
           test t0
           test t1
           mapM_ test t2s
#endif

-- infix 4 ===
-- (===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
-- (f === g) x = f x == g x

infix 4 =~=
(=~=) :: ApproxEq b => (a -> b) -> (a -> b) -> a -> Bool
(f =~= g) x = f x =~ g x

fftIsDftL :: (FFT f, Foldable f, Foldable (FFO f), RealFloat a, ApproxEq a) =>
             f (Complex a) -> Bool
fftIsDftL = toList . fft =~= dftL . toList

dftTIsDftL :: (AFS f, Traversable f, RealFloat a, ApproxEq a) =>
              f (Complex a) -> Bool
dftTIsDftL = toList . dftT =~= dftL . toList

dftIsDftL :: (AFS f, RealFloat a, ApproxEq a) =>
             f (Complex a) -> Bool
dftIsDftL = toList . dft =~= dftL . toList

-- -- TEMP:
-- dftDft :: (AFS f, Traversable f, RealFloat a, ApproxEq a) =>
--           f (Complex a) -> ([Complex a], [Complex a])
-- dftDft xs = (toList . dft $ xs, dftL . toList $ xs)

{--------------------------------------------------------------------
    Properties to test
--------------------------------------------------------------------}

transposeTwiddleCommutes :: (AFS g, Traversable g, AFS f, (ApproxEq (f (g C))))
                         => g (f C) -> Bool
transposeTwiddleCommutes =
 twiddle . transpose =~= transpose . twiddle

prop_transposeTwiddle_L3P :: LTree N3 (Pair C) -> Bool
prop_transposeTwiddle_L3P = transposeTwiddleCommutes

prop_transposeTwiddle_R3P :: RTree N3 (Pair C) -> Bool
prop_transposeTwiddle_R3P = transposeTwiddleCommutes

-- dft tests fail. Hm!

-- prop_dft_R3 :: RTree N3 C -> Bool
-- prop_dft_R3 = dftIsDftL

-- prop_dft_L3 :: LTree N3 C -> Bool
-- prop_dft_L3 = dftIsDftL

prop_dftT_p :: Pair C -> Bool
prop_dftT_p = dftTIsDftL

prop_dftT_L3 :: LTree N3 C -> Bool
prop_dftT_L3 = dftTIsDftL

prop_dftT_R3 :: RTree N3 C -> Bool
prop_dftT_R3 = dftTIsDftL

prop_fft_p :: Pair C -> Bool
prop_fft_p = fftIsDftL

prop_fft_L1 :: LTree N1 C -> Bool
prop_fft_L1 = fftIsDftL

prop_fft_L2 :: LTree N2 C -> Bool
prop_fft_L2 = fftIsDftL

prop_fft_L3 :: LTree N3 C -> Bool
prop_fft_L3 = fftIsDftL

prop_fft_L4 :: LTree N4 C -> Bool
prop_fft_L4 = fftIsDftL

prop_fft_R1 :: RTree N1 C -> Bool
prop_fft_R1 = fftIsDftL

prop_fft_R2 :: RTree N2 C -> Bool
prop_fft_R2 = fftIsDftL

prop_fft_R3 :: RTree N3 C -> Bool
prop_fft_R3 = fftIsDftL

prop_fft_R4 :: RTree N4 C -> Bool
prop_fft_R4 = fftIsDftL

-- TH oddity
return []

runTests :: IO Bool
runTests = $quickCheckAll

-- end of tests
#endif
