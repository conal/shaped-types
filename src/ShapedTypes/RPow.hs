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

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.RPow
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Right-associated functor exponentiation
----------------------------------------------------------------------

-- #define UseGenerics

module ShapedTypes.RPow where

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
#ifndef UseGenerics
import Control.Arrow (first)
#endif
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
data RPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> RPow h Z a
  B :: h (RPow h n a) -> RPow h (S n) a

-- I use "RPow" instead of "Pow" to make compiler output easier to follow.

type (^) = RPow

unL :: RPow h Z a -> a
unL (L a) = a

unB :: RPow h (S n) a -> h (RPow h n a)
unB (B p) = p

inL :: (a -> b) -> (RPow h Z a -> RPow h Z b)
inL = L <~ unL

inB :: (h (RPow h m a) -> h (RPow h n b))
    -> (RPow h (S m) a -> RPow h (S n) b)
inB = B <~ unB

inL2 :: (a -> b -> c) -> (RPow h Z a -> RPow h Z b -> RPow h Z c)
inL2 = inL <~ unL

inB2 :: (h (RPow h m a) -> h (RPow h n b) -> h (RPow h o c))
     -> (RPow h (S m) a -> RPow h (S n) b -> RPow h (S o) c)
inB2 = inB <~ unB

{--------------------------------------------------------------------
    Standard type class instances
--------------------------------------------------------------------}

instance Functor (RPow h Z) where
  fmap f (L a ) = L (f a)
  {-# INLINABLE fmap #-}

instance (Functor h, Functor (RPow h n)) => Functor (RPow h (S n)) where
  fmap f (B ts) = B ((fmap.fmap) f ts)
  {-# INLINABLE fmap #-}
  SPECS(Functor)

instance Applicative (RPow h Z) where
  pure a = L a
  L f <*> L a = L (f a)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance (Applicative h, Applicative (RPow h n)) => Applicative (RPow h (S n)) where
  pure a = B (pure (pure a))
  B fs <*> B xs = B (liftA2 (<*>) fs xs)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (RPow h Z) where
  foldMap f (L a) = f a
  {-# INLINABLE foldMap #-}

instance (Foldable h, Foldable (RPow h n)) => Foldable (RPow h (S n)) where
  foldMap f (B ts) = (foldMap.foldMap) f ts
  {-# INLINABLE foldMap #-}
  SPECS(Foldable)

instance Traversable (RPow h Z) where
  traverse f (L a ) = L <$> f a
  {-# INLINABLE traverse #-}

instance (Traversable h, Traversable (RPow h n)) => Traversable (RPow h (S n)) where
  traverse f (B ts) = B <$> (traverse.traverse) f ts
  {-# INLINABLE traverse #-}
  SPECS(Traversable)

instance Eq a => Eq (RPow h Z a) where
  (==) = (==) `on` unL

instance Eq (h (RPow h n a)) => Eq (RPow h (S n) a) where
  (==) = (==) `on` unB

instance Ord a => Ord (RPow h Z a) where
  compare = compare `on` unL

instance Ord (h (RPow h n a)) => Ord (RPow h (S n) a) where
  compare = compare `on` unB

instance Show a => Show (RPow h Z a) where
  showsPrec p (L a)  = showsUnary "L" p a

instance Show (h (RPow h n a)) => Show (RPow h (S n) a) where
  showsPrec p (B ts)  = showsUnary "B" p ts

instance Arbitrary a => Arbitrary (RPow h Z a) where
  arbitrary    = L <$> arbitrary
  shrink (L a) = L <$> shrink a

instance Arbitrary (h (RPow h n a)) => Arbitrary (RPow h (S n) a) where
  arbitrary    = B <$> arbitrary
  shrink (B a) = B <$> shrink a

instance CoArbitrary a => CoArbitrary (RPow h Z a) where
  coarbitrary (L a) = coarbitrary a

instance CoArbitrary (h (RPow h n a)) => CoArbitrary (RPow h (S n) a) where
  coarbitrary (B a) = coarbitrary a

{--------------------------------------------------------------------
    keys package
--------------------------------------------------------------------}

type instance Key (RPow h m) = Vec m (Key h)

instance Keyed (RPow h Z) where
  mapWithKey q = inL (q ZVec)

instance (Keyed h, Keyed (RPow h n)) => Keyed (RPow h (S n)) where
  mapWithKey q = inB (mapWithKey (mapWithKey . fmap q . (:<)))

#if 0
mapWithKey :: (Key f -> a -> b) -> f a -> f b
           :: (Key (RPow h (S n)) -> a -> b) -> RPow h (S n) a -> RPow h (S n) b
           :: (Vec (S n) h -> a -> b) -> RPow h (S n) a -> RPow h (S n) b

q :: Vec (S n) h -> a -> b
ts :: h (RPow n a)
hk :: Key h
tk :: Key (RPow n h)
   :: Vec n (Key h)

hk :< tk :: Vec (S n) (Key h)
         :: Key (RPow (S n) h)

mapWithKey q
  = \ (B ts) -> B (mapWithKey (\ hk -> mapWithKey (\ tk a -> q (hk :< tk) a)) ts)
  = inB $ mapWithKey (\ hk -> mapWithKey (\ tk a -> q (hk :< tk) a))
  = inB $ mapWithKey (\ hk -> mapWithKey (\ tk -> q (hk :< tk)))
  = inB $ mapWithKey (\ hk -> mapWithKey (q . (hk :<)))
  = inB $ mapWithKey (\ hk -> mapWithKey . (q .) $ (hk :<))
  = inB $ mapWithKey (\ hk -> mapWithKey . (q .) $ (:<) hk)
  = inB (mapWithKey (mapWithKey . fmap q . (:<)))

#endif

instance (Functor (RPow n h), Applicative (RPow n h)) => Zip (RPow n h) where
  zipWith = liftA2

-- Without the seemingly redundant Functor (Vec n) constraint, GHC 8.1.20160307 says
-- 
--     • Could not deduce (Functor (Vec n))
--         arising from the superclasses of an instance declaration
--       from the context: Applicative (Vec n)
--         bound by the instance declaration
--         at ShapedTypes/Vec.hs:(154,10)-(155,13)
--
-- Perhaps <https://ghc.haskell.org/trac/ghc/ticket/11427>.

instance (Applicative (RPow n h), Keyed (RPow n h)) => ZipWithKey (RPow n h)

instance Indexable (RPow h n) => Lookup (RPow h n) where
  lookup k t = Just (index t k)

instance Indexable (RPow h Z) where
  index (L a) ZVec = a
instance (Indexable h, Indexable (RPow h n)) => Indexable (RPow h (S n)) where
  index (B ts) (k :< ks) = ts ! k ! ks

instance Adjustable (RPow h Z) where
  adjust f ZVec = inL f

instance (Adjustable h, Adjustable (RPow h n)) => Adjustable (RPow h (S n)) where
  adjust f (k :< ks) = inB (adjust (adjust f ks) k)

#if 0
f :: Unop a
k :: Key h
ks :: Vec n (Key h)

adjust f (k :< ks) (B ts) = B (adjust (adjust f ks) k ts)
adjust f (k :< ks) = \ (B ts) -> B (adjust (adjust f ks) k ts)
adjust f (k :< ks) = inB (adjust (adjust f ks) k)
#endif

{- -- Worth implementing?

instance (Foldable (RPow n h), Keyed (RPow n h)) => FoldableWithKey (RPow n h) where
  foldMapWithKey f = foldMap (uncurry f) . keyed

instance (Traversable (RPow n h), Keyed (RPow n h)) => TraversableWithKey (RPow n h) where
  traverseWithKey f = traverse (uncurry f) . keyed

-}

{--------------------------------------------------------------------
    Other representations
--------------------------------------------------------------------}

instance Generic1 (RPow h Z) where
  type Rep1 (RPow h Z) = Par1
  from1 = Par1 . unL
  to1   = L . unPar1

instance Generic1 (RPow h (S n)) where
  type Rep1 (RPow h (S n)) = h :.: RPow h n
  from1 = Comp1 . unB
  to1   = B . unComp1

instance HasRep (RPow h Z a) where
  type Rep (RPow h Z a) = a
  repr (L a) = a
  abst = L

instance HasRep (RPow h (S n) a) where
  type Rep (RPow h (S n) a) = h (RPow h n a)
  repr (B ts) = ts
  abst = B

{--------------------------------------------------------------------
    shaped-types instances
--------------------------------------------------------------------}

instance (Foldable (RPow h n), ApproxEq a) => ApproxEq (RPow h n a) where
  (=~) = approxEqFoldable

-- -- Compute size @h exactly once where genericSize would compute it n times.
-- instance (Sized h, Foldable (Vec n), Applicative (Vec n))
--       => Sized (RPow h n) where
--   size = product (pure (size @h) :: Vec n Int)
--   {-# INLINE size #-}

instance (Sized h, Sized (Vec n)) => Sized (RPow h n) where
  size = size @h ^ size @(Vec n)
  {-# INLINE size #-}

#ifdef UseGenerics

instance (Generic1 (RPow h n), LScan (Rep1 (RPow h n))) => LScan (RPow h n) where
  lscan = genericLscan
  {-# INLINE lscan #-}

#else

instance LScan (RPow h Z) where
  lscan (L a) = (L mempty, a)
  {-# INLINE lscan #-}
instance (LScan h, Zip h, LFScan (RPow h n)) => LScan (RPow h (S n)) where
  lscan (B ts) = first B (lscanComp ts)
  {-# INLINE lscan #-}

#endif

{--------------------------------------------------------------------
    Circuit support
--------------------------------------------------------------------}

AbsTy(RPow h   Z   a)
AbsTy(RPow h (S n) a)
