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

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
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

#define UseGenerics

module ShapedTypes.LPow where

-- TODO: Explicit exports

#define SPEC(cls,n) {-# SPECIALISE instance cls (h ^ (n)) #-}

#define SPECS(cls) \
--   SPEC(cls,N1); SPEC(cls,N2); SPEC(cls,N3); SPEC(cls,N4);\
--   SPEC(cls,N5); SPEC(cls,N6); SPEC(cls,N7); SPEC(cls,N8)

-- The more specializations we declare here, the more time it takes to compile
-- this library code *and* the less time it takes to compile client code. We
-- thus probably want to comment out all or some of the `SPEC`s in `SPECS` while
-- developing.

AbsTyImports

import Data.Function (on)
import Control.Arrow (first)
import Control.Applicative (liftA2)
import GHC.Generics (Generic1(..),Par1(..),(:.:)(..))
import Test.QuickCheck (Arbitrary(..),CoArbitrary(..))

import Data.Key

import Circat.Rep
import Circat.Misc ((<~),showsUnary)
import Circat.ApproxEq

import ShapedTypes.Sized
import ShapedTypes.Nat hiding (type (^))
import ShapedTypes.Vec (Vec(..))
import ShapedTypes.Scan

{--------------------------------------------------------------------
    Type and basic manipulation
--------------------------------------------------------------------}

infix 8 ^  -- infixr is ill-kinded, while infixl is contrary to convention

-- Top-down, depth-typed, perfect, binary, leaf trees
data Pow :: (* -> *) -> Nat -> * -> * where
  L :: a -> Pow h Z a
  B :: Pow h n (h a) -> Pow h (S n) a

type (^) = Pow

unL :: Pow h Z a -> a
unL (L a) = a

unB :: Pow h (S n) a -> Pow h n (h a)
unB (B p) = p

inL :: (a -> b) -> (Pow h Z a -> Pow h Z b)
inL = L <~ unL

inB :: (Pow h m (h a) -> Pow h n (h b))
    -> (Pow h (S m) a -> Pow h (S n) b)
inB = B <~ unB

inL2 :: (a -> b -> c) -> (Pow h Z a -> Pow h Z b -> Pow h Z c)
inL2 = inL <~ unL

inB2 :: (Pow h m (h a) -> Pow h n (h b) -> Pow h o (h c))
     -> (Pow h (S m) a -> Pow h (S n) b -> Pow h (S o) c)
inB2 = inB <~ unB

{--------------------------------------------------------------------
    Standard type class instances
--------------------------------------------------------------------}

instance Functor (Pow h Z) where
  fmap f (L a ) = L (f a)
  {-# INLINABLE fmap #-}

instance (Functor h, Functor (Pow h n)) => Functor (Pow h (S n)) where
  fmap f (B ts) = B ((fmap.fmap) f ts)
  {-# INLINABLE fmap #-}
  SPECS(Functor)

instance Applicative (Pow h Z) where
  pure a = L a
  L f <*> L a = L (f a)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance (Applicative h, Applicative (Pow h n)) => Applicative (Pow h (S n)) where
  pure a = B (pure (pure a))
  B fs <*> B xs = B (liftA2 (<*>) fs xs)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (Pow h Z) where
  foldMap f (L a) = f a
  {-# INLINABLE foldMap #-}

instance (Foldable h, Foldable (Pow h n)) => Foldable (Pow h (S n)) where
  foldMap f (B ts) = (foldMap.foldMap) f ts
  {-# INLINABLE foldMap #-}
  SPECS(Foldable)

instance Traversable (Pow h Z) where
  traverse f (L a ) = L <$> f a
  {-# INLINABLE traverse #-}

instance (Traversable h, Traversable (Pow h n)) => Traversable (Pow h (S n)) where
  traverse f (B ts) = B <$> (traverse.traverse) f ts
  {-# INLINABLE traverse #-}
  SPECS(Traversable)

instance Eq a => Eq (Pow h Z a) where
  (==) = (==) `on` unL

instance Eq (Pow h n (h a)) => Eq (Pow h (S n) a) where
  (==) = (==) `on` unB

instance Ord a => Ord (Pow h Z a) where
  compare = compare `on` unL

instance Ord (Pow h n (h a)) => Ord (Pow h (S n) a) where
  compare = compare `on` unB

instance Show a => Show (Pow h Z a) where
  showsPrec p (L a)  = showsUnary "L" p a

instance Show (Pow h n (h a)) => Show (Pow h (S n) a) where
  showsPrec p (B ts)  = showsUnary "B" p ts

instance Arbitrary a => Arbitrary (Pow h Z a) where
  arbitrary    = L <$> arbitrary
  shrink (L a) = L <$> shrink a

instance Arbitrary (Pow h n (h a)) => Arbitrary (Pow h (S n) a) where
  arbitrary    = B <$> arbitrary
  shrink (B a) = B <$> shrink a

instance CoArbitrary a => CoArbitrary (Pow h Z a) where
  coarbitrary (L a) = coarbitrary a

instance CoArbitrary (Pow h n (h a)) => CoArbitrary (Pow h (S n) a) where
  coarbitrary (B a) = coarbitrary a

{--------------------------------------------------------------------
    keys package
--------------------------------------------------------------------}

type instance Key (Pow h m) = Vec m (Key h)

-- TODO: Consider using snoc vectors

instance Keyed (Pow h Z) where
  mapWithKey q = inL (q ZVec)

instance (Keyed h, Keyed (Pow h n)) => Keyed (Pow h (S n)) where
  mapWithKey q = inB (mapWithKey (mapWithKey . fmap q . flip (:<)))

-- See RPow source for typings

instance (Functor (Pow n h), Applicative (Pow n h)) => Zip (Pow n h) where
  zipWith = liftA2

-- Without the seemingly redundant Functor (Vec n) constraint, GHC 8.1.20160307 says
-- 
--     • Could not deduce (Functor (Pow n h))
--         arising from the superclasses of an instance declaration
--       from the context: Applicative (Pow n h)
--         bound by the instance declaration
--         at src/ShapedTypes/LPow.hs:202:10-47
--
-- Perhaps <https://ghc.haskell.org/trac/ghc/ticket/11427>.

instance (Applicative (Pow n h), Keyed (Pow n h)) => ZipWithKey (Pow n h)

instance Indexable (Pow h n) => Lookup (Pow h n) where
  lookup k t = Just (index t k)

instance Indexable (Pow h Z) where
  index (L a) ZVec = a

instance (Indexable h, Indexable (Pow h n)) => Indexable (Pow h (S n)) where
  index (B ts) (k :< ks) = ts ! ks ! k

instance Adjustable (Pow h Z) where
  adjust f ZVec = inL f

instance (Adjustable h, Adjustable (Pow h n)) => Adjustable (Pow h (S n)) where
  adjust f (k :< ks) = inB (adjust (adjust f k) ks)

{- -- Worth implementing?

instance (Foldable (Pow n h), Keyed (Pow n h)) => FoldableWithKey (Pow n h) where
  foldMapWithKey f = foldMap (uncurry f) . keyed

instance (Traversable (Pow n h), Keyed (Pow n h)) => TraversableWithKey (Pow n h) where
  traverseWithKey f = traverse (uncurry f) . keyed
-}

{--------------------------------------------------------------------
    Other representations
--------------------------------------------------------------------}

instance Generic1 (Pow h Z) where
  type Rep1 (Pow h Z) = Par1
  from1 = Par1 . unL
  to1   = L . unPar1

instance Generic1 (Pow h (S n)) where
  type Rep1 (Pow h (S n)) = Pow h n :.: h
  from1 = Comp1 . unB
  to1   = B . unComp1

instance HasRep (Pow h Z a) where
  type Rep (Pow h Z a) = a
  repr (L a) = a
  abst = L

instance HasRep (Pow h (S n) a) where
  type Rep (Pow h (S n) a) = Pow h n (h a)
  repr (B t) = t
  abst t = B t

{--------------------------------------------------------------------
    shaped-types instances
--------------------------------------------------------------------}

instance (Foldable (Pow h n), ApproxEq a) => ApproxEq (Pow h n a) where
  (=~) = approxEqFoldable

-- -- Compute size @h exactly once where genericSize would compute it n times.
-- instance (Sized h, Foldable (Vec n), Applicative (Vec n))
--       => Sized (Pow h n) where
--   size = product (pure (size @h)  :: Vec n Int)

instance (Sized h, Sized (Vec n)) => Sized (Pow h n) where
  size = size @h ^ size @(Vec n)
  {-# INLINE size #-}

#ifdef UseGenerics

instance (Generic1 (Pow h n), LScan (Rep1 (Pow h n))) => LScan (Pow h n) where
  lscan = genericLscan
  {-# INLINE lscan #-}

#else

instance LScan (Pow h Z) where
  lscan (L a) = (L mempty, a)
  {-# INLINE lscan #-}
instance (LFScan h, LScan (Pow h n), Applicative (Pow h n))
      => LScan (Pow h (S n)) where
  lscan (B ts) = first B (lscanComp ts)
  {-# INLINE lscan #-}

#endif

{--------------------------------------------------------------------
    Circuit support
--------------------------------------------------------------------}

AbsTy(Pow h   Z   a)
AbsTy(Pow h (S n) a)
