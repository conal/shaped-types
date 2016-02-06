{-# LANGUAGE CPP                #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# LANGUAGE UndecidableInstances #-} -- see below

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.RTree
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some data types for specializing
----------------------------------------------------------------------

{-# OPTIONS_GHC -funfolding-use-threshold=0 -ddump-simpl -ddump-to-file -dppr-case-as-let -dsuppress-module-prefixes -dsuppress-idinfo -dsuppress-uniques -dsuppress-coercions #-}

-- TODO: Revisit these flags, and perhaps move to Makefile.
-- Do I still want -funfolding-use-threshold=0 ?

module ShapedTypes.RTree (Tree(..)) where

#define SPEC(cls,n) {-# SPECIALISE instance cls (Tree n) #-}

#define SPECS(cls) \
  SPEC(cls,N1); SPEC(cls,N2); SPEC(cls,N3); SPEC(cls,N4);\
--   SPEC(cls,N5); SPEC(cls,N6); SPEC(cls,N7); SPEC(cls,N8)

-- The more specializations we declare here, the more time it takes to compile
-- this library code *and* the less time it takes to compile client code. We
-- thus probably want to comment out all or some of the `SPEC`s in `SPECS` while
-- developing.

import Control.Applicative (liftA2)

import Circat.Rep

import ShapedTypes.Nat
import ShapedTypes.Pair

-- Top-down, depth-typed, perfect, binary, leaf trees
data Tree :: Nat -> * -> * where
  L :: a -> Tree Z a
  B :: Pair (Tree n a) -> Tree (S n) a

instance Functor (Tree Z) where
  fmap f (L a ) = L (f a)
  {-# INLINABLE fmap #-}

instance Functor (Tree n) => Functor (Tree (S n)) where
  fmap f (B ts) = B ((fmap.fmap) f ts)
  {-# INLINABLE fmap #-}
  SPECS(Functor)

instance Applicative (Tree Z) where
  pure a = L a
  L f <*> L a = L (f a)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance Applicative (Tree n) => Applicative (Tree (S n)) where
  pure a = B (pure (pure a))
  B fs <*> B xs = B (liftA2 (<*>) fs xs)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (Tree Z) where
  foldMap f (L a) = f a
  {-# INLINABLE foldMap #-}

instance Foldable (Tree n) => Foldable (Tree (S n)) where
  foldMap f (B ts) = (foldMap.foldMap) f ts
  {-# INLINABLE foldMap #-}
  SPECS(Foldable)

instance Traversable (Tree Z) where
  traverse f (L a ) = L <$> f a
  {-# INLINABLE traverse #-}

instance Traversable (Tree n) => Traversable (Tree (S n)) where
  traverse f (B ts) = B <$> (traverse.traverse) f ts
  {-# INLINABLE traverse #-}
  SPECS(Traversable)

type instance Rep (Tree Z a) = a
instance HasRep (Tree Z a) where
  repr (L a) = a
  abst = L

#if 1
-- One step at a time:
type instance Rep (Tree (S n) a) = Pair (Tree n a)
instance HasRep (Tree (S n) a) where
  repr(B ts) = ts
  abst = B
#else
-- Two steps:
type instance Rep (Tree (S n) a) = Rep (Pair (Tree n a)) -- *
-- type instance Rep (Tree (S n) a) = (Tree n a , Tree n a)
instance HasRep (Tree (S n) a) where
  repr(B ts) = repr ts
  abst ts = B (abst ts)
#endif

-- *
--     Application is no smaller than the instance head
--       in the type family application: Rep (Pair (Tree n a))
--     (Use UndecidableInstances to permit this)
--     In the type instance declaration for ‘Rep’
