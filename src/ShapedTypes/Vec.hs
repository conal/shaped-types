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
-- Module      :  ShapedTypes.Vec
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Length-typed lists/vectors
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin-opt=LambdaCCC.Reify:verbose #-}

module ShapedTypes.Vec (Vec(..)) where

import Data.Monoid ((<>))
import Control.Applicative (liftA2)

import Circat.Rep

-- import TypeUnary.TyNat
import ShapedTypes.Nat

AbsTyImports

#define SPEC(cls,n) {-# SPECIALISE instance cls (Vec n) #-}

#define SPECS(cls) \
--   SPEC(cls,N1); SPEC(cls,N2); SPEC(cls,N3); SPEC(cls,N4);\
--   SPEC(cls,N5); SPEC(cls,N6); SPEC(cls,N7); SPEC(cls,N8)

-- The more specializations we declare here, the more time it takes to compile
-- this library code *and* the less time it takes to compile client code. We
-- thus probably want to comment out all or some of the `SPEC`s in `SPECS` while
-- developing.

infixr 5 :<

-- | Vectors with type-determined length, having empty vector ('ZVec') and
-- vector cons ('(:<)').
data Vec :: Nat -> * -> * where
  ZVec :: Vec Z a 
  (:<) :: a -> Vec n a -> Vec (S n) a
-- deriving Typeable

instance Functor (Vec Z) where
  fmap _ ZVec = ZVec
  {-# INLINABLE fmap #-}

instance Functor (Vec n) => Functor (Vec (S n)) where
  fmap f (a :< u) = f a :< fmap f u
  {-# INLINABLE fmap #-}
  SPECS(Functor)

instance Applicative (Vec Z) where
  pure _ = ZVec
  ZVec <*> ZVec = ZVec
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure a = a :< pure a
  (f :< fs) <*> (a :< as) = f a :< (fs <*> as)
  {-# INLINABLE pure  #-}
  {-# INLINABLE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (Vec Z) where
  foldMap _ ZVec = mempty
  {-# INLINABLE foldMap #-}

instance Foldable (Vec n) => Foldable (Vec (S n)) where
  foldMap h (a :< as) = h a <> foldMap h as
  {-# INLINABLE foldMap #-}
  SPECS(Foldable)

instance Traversable (Vec Z) where
  traverse _ ZVec = pure ZVec
  {-# INLINABLE traverse #-}

instance Traversable (Vec n) => Traversable (Vec (S n)) where
  traverse f (a :< as) = liftA2 (:<) (f a) (traverse f as)
  {-# INLINABLE traverse #-}
  SPECS(Traversable)

type instance Rep (Vec Z a) = ()
instance HasRep (Vec Z a) where
  repr ZVec = ()
  abst () = ZVec

type instance Rep (Vec (S n) a) = (a,Vec n a)
instance HasRep (Vec (S n) a) where
  repr (a :< as) = (a, as)
  abst (a, as) = (a :< as)


AbsTy(Vec Z a)
AbsTy(Vec (S n) a)
