{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

#include "Circat/AbsTy.inc"

AbsTyPragmas

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.LPow
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Right-associated functor exponentiation
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin=ReificationRules.Plugin -dcore-lint -fexpose-all-unfoldings #-}

-- {-# OPTIONS_GHC -fplugin-opt=ReificationRules.Plugin:trace #-}
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

-- #define UseGenerics

module ShapedTypes.LPow
  (
    LPow(..),Pow,type (^)
  , unL,unB, inL,inB, inL2,inB2
  ) where

-- TODO: Explicit exports

#define SPEC(cls,n) {-# SPECIALISE instance cls (h ^ (n)) #-}

#define SPECS(cls) \
--   SPEC(cls,N1); SPEC(cls,N2); SPEC(cls,N3); SPEC(cls,N4);\
--   SPEC(cls,N5); SPEC(cls,N6); SPEC(cls,N7); SPEC(cls,N8)

-- The more specializations we declare here, the more time it takes to compile
-- this library code *and* the less time it takes to compile client code. We
-- thus probably want to comment out all or some of the `SPEC`s in `SPECS` while
-- developing.

import Data.Function (on)
import Control.Arrow (first)
import Control.Applicative (liftA2)
import GHC.Generics (Generic1(..),Par1(..),(:.:)(..))
import Test.QuickCheck (Arbitrary(..),CoArbitrary(..))

import Data.Key

import Circat.Misc ((<~),showsUnary)

import ShapedTypes.ApproxEq
import ShapedTypes.Sized
import ShapedTypes.Nat hiding (type (^))
import ShapedTypes.Vec (Vec(..))
import ShapedTypes.Scan

import ShapedTypes.Types.LPow

{--------------------------------------------------------------------
    Type and basic manipulation
--------------------------------------------------------------------}

infix 8 ^  -- infixr is ill-kinded, while infixl is contrary to convention

type Pow = LPow

type (^) = LPow

unL :: LPow h Z a -> a
unL (L a) = a

unB :: LPow h (S n) a -> LPow h n (h a)
unB (B p) = p

inL :: (a -> b) -> (LPow h Z a -> LPow h Z b)
inL = L <~ unL

inB :: (LPow h m (h a) -> LPow h n (h b))
    -> (LPow h (S m) a -> LPow h (S n) b)
inB = B <~ unB

inL2 :: (a -> b -> c) -> (LPow h Z a -> LPow h Z b -> LPow h Z c)
inL2 = inL <~ unL

inB2 :: (LPow h m (h a) -> LPow h n (h b) -> LPow h o (h c))
     -> (LPow h (S m) a -> LPow h (S n) b -> LPow h (S o) c)
inB2 = inB <~ unB

{--------------------------------------------------------------------
    Standard type class instances
--------------------------------------------------------------------}

instance Functor (LPow h Z) where
  fmap f (L a ) = L (f a)
  {-# INLINE fmap #-}

instance (Functor h, Functor (LPow h n)) => Functor (LPow h (S n)) where
  fmap f (B ts) = B ((fmap.fmap) f ts)
  {-# INLINE fmap #-}
  SPECS(Functor)

instance Applicative (LPow h Z) where
  pure a = L a
  L f <*> L a = L (f a)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance (Applicative h, Applicative (LPow h n)) => Applicative (LPow h (S n)) where
  pure a = B (pure (pure a))
  B fs <*> B xs = B (liftA2 (<*>) fs xs)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (LPow h Z) where
  foldMap f (L a) = f a
  {-# INLINE foldMap #-}

instance (Foldable h, Foldable (LPow h n)) => Foldable (LPow h (S n)) where
  foldMap f (B ts) = (foldMap.foldMap) f ts
  {-# INLINE foldMap #-}
  SPECS(Foldable)

instance Traversable (LPow h Z) where
  traverse f (L a ) = L <$> f a
  {-# INLINE traverse #-}

instance (Traversable h, Traversable (LPow h n)) => Traversable (LPow h (S n)) where
  traverse f (B ts) = B <$> (traverse.traverse) f ts
  {-# INLINE traverse #-}
  SPECS(Traversable)

instance Eq a => Eq (LPow h Z a) where
  (==) = (==) `on` unL

instance Eq (LPow h n (h a)) => Eq (LPow h (S n) a) where
  (==) = (==) `on` unB

instance Ord a => Ord (LPow h Z a) where
  compare = compare `on` unL

instance Ord (LPow h n (h a)) => Ord (LPow h (S n) a) where
  compare = compare `on` unB

instance Show a => Show (LPow h Z a) where
  showsPrec p (L a)  = showsUnary "L" p a

instance Show (LPow h n (h a)) => Show (LPow h (S n) a) where
  showsPrec p (B ts)  = showsUnary "B" p ts

instance Arbitrary a => Arbitrary (LPow h Z a) where
  arbitrary    = L <$> arbitrary
  shrink (L a) = L <$> shrink a

instance Arbitrary (LPow h n (h a)) => Arbitrary (LPow h (S n) a) where
  arbitrary    = B <$> arbitrary
  shrink (B a) = B <$> shrink a

instance CoArbitrary a => CoArbitrary (LPow h Z a) where
  coarbitrary (L a) = coarbitrary a

instance CoArbitrary (LPow h n (h a)) => CoArbitrary (LPow h (S n) a) where
  coarbitrary (B a) = coarbitrary a

{--------------------------------------------------------------------
    keys package
--------------------------------------------------------------------}

type instance Key (LPow h m) = Vec m (Key h)

-- TODO: Consider using snoc vectors

instance Keyed (LPow h Z) where
  mapWithKey q = inL (q ZVec)

instance (Keyed h, Keyed (LPow h n)) => Keyed (LPow h (S n)) where
  mapWithKey q = inB (mapWithKey (mapWithKey . fmap q . flip (:<)))

-- See RPow source for typings

instance (Functor (LPow n h), Applicative (LPow n h)) => Zip (LPow n h) where
  zipWith = liftA2

-- Without the seemingly redundant Functor (Vec n) constraint, GHC 8.1.20160307 says
-- 
--     • Could not deduce (Functor (LPow n h))
--         arising from the superclasses of an instance declaration
--       from the context: Applicative (LPow n h)
--         bound by the instance declaration
--         at src/ShapedTypes/LPow.hs:202:10-47
--
-- Perhaps <https://ghc.haskell.org/trac/ghc/ticket/11427>.

instance (Applicative (LPow n h), Keyed (LPow n h)) => ZipWithKey (LPow n h)

instance Indexable (LPow h n) => Lookup (LPow h n) where
  lookup k t = Just (index t k)

instance Indexable (LPow h Z) where
  index (L a) ZVec = a

instance (Indexable h, Indexable (LPow h n)) => Indexable (LPow h (S n)) where
  index (B ts) (k :< ks) = ts ! ks ! k

instance Adjustable (LPow h Z) where
  adjust f ZVec = inL f

instance (Adjustable h, Adjustable (LPow h n)) => Adjustable (LPow h (S n)) where
  adjust f (k :< ks) = inB (adjust (adjust f k) ks)

{- -- Worth implementing?

instance (Foldable (LPow n h), Keyed (LPow n h)) => FoldableWithKey (LPow n h) where
  foldMapWithKey f = foldMap (uncurry f) . keyed

instance (Traversable (LPow n h), Keyed (LPow n h)) => TraversableWithKey (LPow n h) where
  traverseWithKey f = traverse (uncurry f) . keyed
-}

{--------------------------------------------------------------------
    Other representations
--------------------------------------------------------------------}

instance Generic1 (LPow h Z) where
  type Rep1 (LPow h Z) = Par1
  from1 = Par1 . unL
  to1   = L . unPar1

instance Generic1 (LPow h (S n)) where
  type Rep1 (LPow h (S n)) = LPow h n :.: h
  from1 = Comp1 . unB
  to1   = B . unComp1

{--------------------------------------------------------------------
    shaped-types instances
--------------------------------------------------------------------}

instance (Foldable (LPow h n), ApproxEq a) => ApproxEq (LPow h n a) where
  (=~) = approxEqFoldable

-- -- Compute size @h exactly once where genericSize would compute it n times.
-- instance (Sized h, Foldable (Vec n), Applicative (Vec n))
--       => Sized (LPow h n) where
--   size = product (pure (size @h)  :: Vec n Int)

instance (Sized h, Sized (Vec n)) => Sized (LPow h n) where
  size = size @h ^ size @(Vec n)
  {-# INLINE size #-}

#ifdef UseGenerics

instance (Generic1 (LPow h n), LScan (Rep1 (LPow h n))) => LScan (LPow h n) where
  lscan = genericLscan
  {-# INLINE lscan #-}

#else

instance LScan (LPow h Z) where
  lscan (L a) = (L mempty, a)
  {-# INLINE lscan #-}
instance (LFScan h, LScan (LPow h n), Applicative (LPow h n))
      => LScan (LPow h (S n)) where
  lscan (B ts) = first B (lscanComp ts)
  {-# INLINE lscan #-}

#endif
