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

#include "Circat/AbsTy.inc"

AbsTyPragmas

{-# LANGUAGE UndecidableInstances #-} -- see below

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}

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
-- Some data types for specializing
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin-opt=LambdaCCC.Reify:verbose #-}

module ShapedTypes.LPow (Pow(..),type (^^)) where

#define SPEC(cls,n) {-# SPECIALISE instance cls (Pow n h) #-}

#define SPECS(cls) \
--   SPEC(cls,N1); SPEC(cls,N2); SPEC(cls,N3); SPEC(cls,N4);\
--   SPEC(cls,N5); SPEC(cls,N6); SPEC(cls,N7); SPEC(cls,N8)

-- The more specializations we declare here, the more time it takes to compile
-- this library code *and* the less time it takes to compile client code. We
-- thus probably want to comment out all or some of the `SPEC`s in `SPECS` while
-- developing.

AbsTyImports

import Control.Applicative (liftA2)

import Circat.Rep

import ShapedTypes.Nat
-- import ShapedTypes.Pair

infixr 8 ^^
type (^^) = Pow

-- Top-down, depth-typed, perfect, binary, leaf trees
data Pow :: Nat -> (* -> *) -> * -> * where
  L :: a -> Pow Z h a
  B :: Pow n h (h a) -> Pow (S n) h a

instance Functor (Pow Z h) where
  fmap f (L a ) = L (f a)
  {-# INLINABLE fmap #-}

instance (Functor h, Functor (Pow n h)) => Functor (Pow (S n) h) where
  fmap f (B ts) = B ((fmap.fmap) f ts)
  {-# INLINABLE fmap #-}
  SPECS(Functor)

instance Applicative (Pow Z h) where
  pure a = L a
  L f <*> L a = L (f a)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance (Applicative h, Applicative (Pow n h)) => Applicative (Pow (S n) h) where
  pure a = B (pure (pure a))
  B fs <*> B xs = B (liftA2 (<*>) fs xs)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (Pow Z h) where
  foldMap f (L a) = f a
  {-# INLINABLE foldMap #-}

instance (Foldable h, Foldable (Pow n h)) => Foldable (Pow (S n) h) where
  foldMap f (B ts) = (foldMap.foldMap) f ts
  {-# INLINABLE foldMap #-}
  SPECS(Foldable)

instance Traversable (Pow Z h) where
  traverse f (L a ) = L <$> f a
  {-# INLINABLE traverse #-}

instance (Traversable h, Traversable (Pow n h)) => Traversable (Pow (S n) h) where
  traverse f (B ts) = B <$> (traverse.traverse) f ts
  {-# INLINABLE traverse #-}
  SPECS(Traversable)

type instance Rep (Pow Z h a) = a
instance HasRep (Pow Z h a) where
  repr (L a) = a
  abst = L

#if 1
-- One step at a time:
type instance Rep (Pow (S n) h a) = Pow n h (h a)
instance HasRep (Pow (S n) h a) where
  repr (B t) = t
  abst = B
#else
-- Two steps:
type instance Rep (Pow (S n) a) = Rep (h (Pow n a)) -- *
-- type instance Rep (Pow (S n) a) = (Pow n a , Pow n a)
instance HasRep (Pow (S n) a) where
  repr (B t) = repr t
  abst t = B (abst t)
#endif

-- *
--     Application is no smaller than the instance head
--       in the type family application: Rep (h (Pow n a))
--     (Use UndecidableInstances to permit this)
--     In the type instance declaration for ‘Rep’

AbsTy(Pow Z h a)
AbsTy(Pow (S n) h a)
