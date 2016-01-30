{-# LANGUAGE CPP                #-}
{-# LANGUAGE ConstraintKinds    #-}
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

{-# OPTIONS_GHC -Wall #-}

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

module ShapedTypes.Vec where

-- TODO: explicit exports

import Data.Monoid ((<>))
import Control.Applicative (liftA2)

import Circat.Rep

import TypeUnary.TyNat

infixr 5 :<

-- | Vectors with type-determined length, having empty vector ('ZVec') and
-- vector cons ('(:<)').
data Vec :: * -> * -> * where
  ZVec :: Vec Z a 
  (:<) :: a -> Vec n a -> Vec (S n) a
-- deriving Typeable

instance Functor (Vec Z) where
  -- fmap _ ZVec     = ZVec
  fmap = \ _ ZVec -> ZVec
  {-# INLINE fmap #-}

instance Functor (Vec n) => Functor (Vec (S n)) where
  fmap = \ f (a :< u) -> f a :< fmap f u
  {-# INLINE fmap #-}

instance Applicative (Vec Z) where
  pure = \ _ -> ZVec
  {-# INLINE pure #-}
  -- ZVec <*> ZVec = ZVec
  (<*>) = \ ZVec ZVec -> ZVec
  {-# INLINE (<*>) #-}

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  -- pure a = a :< pure a
  pure = \ a -> a :< pure a
  {-# INLINE pure  #-}
  -- (f :< fs) <*> (a :< as) = f a :< (fs <*> as)
  (<*>) = \ (f :< fs) (a :< as) -> f a :< (fs <*> as)
  {-# INLINE (<*>) #-}

-- TODO: Monad

instance Foldable (Vec Z) where
  -- foldMap _ ZVec = mempty
  foldMap = \ _ ZVec -> mempty
  {-# INLINE foldMap #-}

instance Foldable (Vec n) => Foldable (Vec (S n)) where
  foldMap = \ h (a :< as) -> h a <> foldMap h as
  {-# INLINE foldMap #-}

instance Traversable (Vec Z) where
  traverse = \ _ ZVec -> pure ZVec
  {-# INLINE traverse #-}

instance Traversable (Vec n) => Traversable (Vec (S n)) where
  traverse = \ f (a :< as) -> liftA2 (:<) (f a) (traverse f as)
  {-# INLINE traverse #-}

type instance Rep (Vec Z a) = ()
instance HasRep (Vec Z a) where
  repr ZVec = ()
  abst () = ZVec

type instance Rep (Vec (S n) a) = (a,Vec n a)
instance HasRep (Vec (S n) a) where
  repr (a :< as) = (a, as)
  abst (a, as) = (a :< as)
