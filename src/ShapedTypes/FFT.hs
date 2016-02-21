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
{-# LANGUAGE TypeSynonymInstances   #-}

{-# LANGUAGE UndecidableInstances #-} -- See below

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP

-- #define TESTING

#ifdef TESTING
{-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP
#endif

#define GenericPowFFT

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.FFT
-- Copyright   :  (c) 2015 Conal Elliott
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Generic FFT
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin-opt=LambdaCCC.Reify:verbose #-}

module ShapedTypes.FFT
  ( FFT(..), DFTTy, genericFft, GFFT
  -- Temporary while debugging
  , twiddle, twiddles, omega, cis
  ) where

import Prelude hiding (zipWith)
import GHC.Generics hiding (C)

#ifdef TESTING
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.All (quickCheckAll)
#endif

import Data.Key

import Circat.Misc (transpose,Unop)
import Circat.Complex

import ShapedTypes.Misc
import ShapedTypes.Sized
import ShapedTypes.Scan (LScan,lproducts,iota) -- , lsums
#ifndef GenericPowFFT
import ShapedTypes.Nat
#endif
import ShapedTypes.Vec
import qualified ShapedTypes.LPow as L
import qualified ShapedTypes.RPow as R

{--------------------------------------------------------------------
    FFT
--------------------------------------------------------------------}

type DFTTy f f' = forall a. RealFloat a => f (Complex a) -> f' (Complex a)

class FFT f f' | f -> f' where
  fft :: DFTTy f f'
  -- Temporary hack to avoid newtype-like representation.
  fftDummy :: f a
  fftDummy = undefined

-- #define tySize(f) (size (undefined :: (f) ()))
#define tySize(f) (size (pure () :: (f) ()))

type AFS h = (Applicative h, Zip h, Foldable h, Sized h, LScan h)

twiddle :: forall g f a. (AFS g, AFS f, RealFloat a) => Unop (g (f (Complex a)))
twiddle = (zipWith.zipWith) (*) (twiddles (tySize(g :.: f)))
{-# INLINE twiddle #-}

-- Twiddle factors.
twiddles :: (AFS g, AFS f, RealFloat a) => Int -> g (f (Complex a))
twiddles = fmap powers . powers . omega
{-# INLINE twiddles #-}

-- twiddles n = powers <$> powers (omega n)

omega :: (Integral n, RealFloat a) => n -> Complex a
omega n = cis (- 2 * pi / fromIntegral n)
-- omega n = exp (0 :+ (- 2 * pi / fromIntegral n))
-- omega n = exp (- 2 * (0:+1) * pi / fromIntegral n)
{-# INLINE omega #-}

-- | @'exp' (i * a)
cis :: RealFloat a => a -> Complex a
cis a = cos a :+ sin a

-- Powers of x, starting x^0. Uses 'LScan' for log parallel time
powers :: (LScan f, Applicative f, Num a) => a -> f a
powers = fst . lproducts . pure
{-# INLINE powers #-}

-- TODO: Consolidate with powers in TreeTest and rename sensibly. Maybe use
-- "In" and "Ex" suffixes to distinguish inclusive and exclusive cases.

{--------------------------------------------------------------------
    Generic support
--------------------------------------------------------------------}

instance FFT Par1 Par1 where
  fft = id

instance ( Applicative f , Zip f,  Traversable f , Traversable g
         , Applicative f', Zip g', Applicative g', Traversable g'
         , FFT f f', FFT g g', LScan f, LScan g', Sized f, Sized g' )
      => FFT (g :.: f) (f' :.: g') where
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
genericFft :: (Generic1 f, Generic1 f', FFT (Rep1 f) (Rep1 f')) => DFTTy f f'
genericFft = inGeneric1 fft

type GFFT f f' = (Generic1 (f), Generic1 (f'), FFT (Rep1 (f)) (Rep1 (f')))

#define GenericFFT(f,g) instance GFFT (f)(g) => FFT (f)(g) where fft = genericFft

-- Generalization of 'dft' to traversables. Warning: use only on zippy
-- applicatives (not on []).
dftTraversable :: forall f a. (AFS f, Traversable f, RealFloat a) => Unop (f (Complex a))
dftTraversable xs = out <$> indices
 where
   out k = sum (zipWith (\ n x -> x * ok^n) indices xs)
    where ok = om ^ k
   indices = iota :: f Int
   om = omega (tySize(f))

-- TODO: Replace Applicative with Zippable

-- Perhaps dftTraversable isn't very useful. Its result and argument types match, unlike fft.

{--------------------------------------------------------------------
    Specialized FFT instances.
--------------------------------------------------------------------}

-- I put the specific instances here in order to avoid an import loop between
-- the LPow and RPow modules. I'd still like to find an elegant FFT that maps f
-- to f, and then move the instances to RPow and LPow.

#ifdef GenericPowFFT

instance (Applicative (Vec n), Zip (Vec n), Traversable (Vec n)) =>
         FFT (Vec n) (Vec n) where fft = dftTraversable

-- GenericFFT(Vec     n, Vec     n)
GenericFFT(R.Pow h n, L.Pow k n)
GenericFFT(L.Pow h n, R.Pow k n)

#else
type ATS f = (Applicative f, Traversable f, Sized f)

-- TODO: Vec instance

instance FFT (R.Pow h Z) (L.Pow h Z) where
  fft = L.L . R.unL
  {-# INLINE fft #-}

instance ( ATS h, ATS h', ATS (L.Pow h' n), ATS (R.Pow h n), LScan (R.Pow h n), LScan h'
         , FFT h h', FFT (R.Pow h n) (L.Pow h' n) )
      => FFT (R.Pow h ('S n)) (L.Pow h' ('S n)) where
  fft = L.B . unComp1 . fft . Comp1 . R.unB
  {-# INLINE fft #-}

instance FFT (L.Pow h Z) (R.Pow h Z) where
  fft = R.L . L.unL
  {-# INLINE fft #-}

instance ( ATS h, ATS h', ATS (R.Pow h' n), ATS (L.Pow h n), LScan (R.Pow h' n), LScan h
         , FFT h h', FFT (L.Pow h n) (R.Pow h' n) )
      => FFT (L.Pow h ('S n)) (R.Pow h' ('S n)) where
  fft = R.B . unComp1 . fft . Comp1 . L.unB
  {-# INLINE fft #-}

-- TODO: Revisit these constraints, which don't quite have the duality I expected.
#endif

#ifdef TESTING

{--------------------------------------------------------------------
    Simple, quadratic DFT (for specification & testing)
--------------------------------------------------------------------}

-- Adapted from Dave's definition
dft :: RealFloat a => Unop [Complex a]
dft xs = [ sum [ x * ok^n | x <- xs | n <- [0 :: Int ..] ]
         | k <- [0 .. length xs - 1], let ok = om ^ k ]
 where
   om = omega (length xs)

dftQ :: forall f a. (AFS f, RealFloat a) => Unop (f (Complex a))
dftQ as = (<.> as) <$> twiddles (tySize(L.Tree N1 :. Pair))
{-# INLINE dftQ #-}

-- Binary dot product
infixl 7 <.>
(<.>) :: (Foldable f, Applicative f, Num a) => f a -> f a -> a
u <.> v = sum (zipWith (*) u v)

{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

-- > powers 2 :: L.Tree N2 Int
-- B (B (L ((1 :# 2) :# (4 :# 8))))
-- > powers 2 :: L.Tree N3 Int
-- B (B (B (L (((1 :# 2) :# (4 :# 8)) :# ((16 :# 32) :# (64 :# 128))))))

-- PrettyDouble doesn't yet have an Arbitrary instance, so use Double for now
type C = Complex Double

fftl :: (FFT f f', Foldable f', RealFloat a) => f (Complex a) -> [Complex a]
fftl = toList . fft

type LC n = L.Tree n C
type RC n = R.Tree n C

p1 :: Pair C
p1 = 1 :# 0

tw1 :: L.Tree N1 (Pair C)
tw1 = twiddles (tySize(L.Tree N1 :. Pair))

tw2 :: L.Tree N2 (Pair C)
tw2 = twiddles (tySize(L.Tree N2 :. Pair))

-- Adapted from Dave's testing

test :: (FFT f f', Foldable f, Foldable f') => f C -> IO ()
test fx =
  do ps "\nTesting input" xs
     ps "Expected output" (dft xs)
     ps "Actual output  " (toList (fft fx))
 where
   ps label z = putStrLn (label ++ ": " ++ show z)
   xs = toList fx

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

-- infix 4 ===
-- (===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
-- (f === g) x = f x == g x

infix 4 =~=
(=~=) :: ApproxEq b => (a -> b) -> (a -> b) -> a -> Bool
(f =~= g) x = f x =~ g x

fftIsDft :: (FFT f f', Foldable f, Foldable f', RealFloat a, ApproxEq a) =>
            f (Complex a) -> Bool
fftIsDft = toList . fft =~= dft . toList

dftTIsDft :: (AFS f, Traversable f, RealFloat a, ApproxEq a) =>
            f (Complex a) -> Bool
dftTIsDft = toList . dftT =~= dft . toList

dftQIsDft :: (AFS f, Traversable f, RealFloat a, ApproxEq a) =>
            f (Complex a) -> Bool
dftQIsDft = toList . dftQ =~= dft . toList

-- TEMP:
dftQDft :: (AFS f, Traversable f, RealFloat a, ApproxEq a) =>
        f (Complex a) -> ([Complex a], [Complex a])
dftQDft xs = (toList . dftQ $ xs, dft . toList $ xs)

{--------------------------------------------------------------------
    Properties to test
--------------------------------------------------------------------}

transposeTwiddleCommutes :: (AFS g, Traversable g, AFS f, (ApproxEq (f (g C))))
                         => g (f C) -> Bool
transposeTwiddleCommutes =
 twiddle . transpose =~= transpose . twiddle

prop_transposeTwiddle_L3P :: L.Tree N3 (Pair C) -> Bool
prop_transposeTwiddle_L3P = transposeTwiddleCommutes

prop_transposeTwiddle_R3P :: R.Tree N3 (Pair C) -> Bool
prop_transposeTwiddle_R3P = transposeTwiddleCommutes

-- dftQ tests fail. Hm!

-- prop_dftQ_R3 :: R.Tree N3 C -> Bool
-- prop_dftQ_R3 = dftQIsDft

-- prop_dftQ_L3 :: L.Tree N3 C -> Bool
-- prop_dftQ_L3 = dftQIsDft

prop_dftT_p :: Pair C -> Bool
prop_dftT_p = dftTIsDft

prop_dftT_L3 :: L.Tree N3 C -> Bool
prop_dftT_L3 = dftTIsDft

prop_dftT_R3 :: R.Tree N3 C -> Bool
prop_dftT_R3 = dftTIsDft

prop_fft_p :: Pair C -> Bool
prop_fft_p = fftIsDft

prop_fft_L1 :: L.Tree N1 C -> Bool
prop_fft_L1 = fftIsDft

prop_fft_L2 :: L.Tree N2 C -> Bool
prop_fft_L2 = fftIsDft

prop_fft_L3 :: L.Tree N3 C -> Bool
prop_fft_L3 = fftIsDft

prop_fft_L4 :: L.Tree N4 C -> Bool
prop_fft_L4 = fftIsDft

prop_fft_R1 :: R.Tree N1 C -> Bool
prop_fft_R1 = fftIsDft

prop_fft_R2 :: R.Tree N2 C -> Bool
prop_fft_R2 = fftIsDft

prop_fft_R3 :: R.Tree N3 C -> Bool
prop_fft_R3 = fftIsDft

prop_fft_R4 :: R.Tree N4 C -> Bool
prop_fft_R4 = fftIsDft

-- TH oddity
return []

runTests :: IO Bool
runTests = $quickCheckAll

-- end of tests
#endif
