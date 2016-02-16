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
-- Some data types for specializing
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin-opt=LambdaCCC.Reify:verbose #-}

module ShapedTypes.RPow ((^)(..)) where

#define SPEC(cls,n) {-# SPECIALISE instance cls (h ^ (n)) #-}

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

import ShapedTypes.Nat hiding (type (^))

infix 8 ^  -- infixr is ill-kinded, while infixl is contrary to convention

-- Top-down, depth-typed, perfect, binary, leaf trees
data (^) :: (* -> *) -> Nat -> * -> * where
  L :: a -> (h ^ Z) a
  B :: h ((h ^ n) a) -> (h ^ S n) a

instance Functor (h ^ Z) where
  fmap f (L a ) = L (f a)
  {-# INLINABLE fmap #-}

instance (Functor h, Functor (h ^ n)) => Functor (h ^ S n) where
  fmap f (B ts) = B ((fmap.fmap) f ts)
  {-# INLINABLE fmap #-}
  SPECS(Functor)

instance Applicative (h ^ Z) where
  pure a = L a
  L f <*> L a = L (f a)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance (Applicative h, Applicative (h ^ n)) => Applicative (h ^ S n) where
  pure a = B (pure (pure a))
  B fs <*> B xs = B (liftA2 (<*>) fs xs)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (h ^ Z) where
  foldMap f (L a) = f a
  {-# INLINABLE foldMap #-}

instance (Foldable h, Foldable (h ^ n)) => Foldable (h ^ S n) where
  foldMap f (B ts) = (foldMap.foldMap) f ts
  {-# INLINABLE foldMap #-}
  SPECS(Foldable)

instance Traversable (h ^ Z) where
  traverse f (L a ) = L <$> f a
  {-# INLINABLE traverse #-}

instance (Traversable h, Traversable (h ^ n)) => Traversable (h ^ S n) where
  traverse f (B ts) = B <$> (traverse.traverse) f ts
  {-# INLINABLE traverse #-}
  SPECS(Traversable)

type instance Rep ((h ^ Z) a) = a
instance HasRep ((h ^ Z) a) where
  repr (L a) = a
  abst = L

type instance Rep ((h ^ S n) a) = h ((h ^ n) a)
instance HasRep ((h ^ S n) a) where
  repr (B ts) = ts
  abst = B

AbsTy((h ^  Z ) a)
AbsTy((h ^ S n) a)
