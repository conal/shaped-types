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
-- Module      :  Types
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some data types for specializing
----------------------------------------------------------------------

module Types where

-- TODO: explicit exports

import Control.Applicative (liftA2)

-- import Data.Typeable (Typeable)
-- import Data.Data (Data)

import TypeUnary.TyNat (Z,S)

-- -- Type-level naturals, old-school
-- data Z
-- data S n

infixl 1 :#
-- | Uniform pairs
data Pair a = a :# a deriving (Functor,Traversable) -- ,Eq,Show,Typeable,Data,Generic,Generic1

instance Applicative Pair where
  pure a = a :# a
  (f :# g) <*> (a :# b) = (f a :# g b)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Pair where
  return = pure
  m >>= f = joinP (f <$> m)
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

-- The derived foldMap inserts a mempty (in GHC 7.0.4).
instance Foldable Pair where
  foldMap f (a :# b) = f a `mappend` f b
  {-# INLINE foldMap #-}

joinP :: Pair (Pair a) -> Pair a
joinP ((a :# _) :# (_ :# d)) = a :# d
{-# INLINE joinP #-}

-- Top-down, depth-typed, perfect, binary, leaf trees
data Tree :: * -> * -> * where
  L :: a -> Tree Z a
  B :: Pair (Tree n a) -> Tree (S n) a

instance Functor (Tree Z) where
  fmap f (L a ) = L (f a)
  {-# INLINABLE fmap #-}

instance Functor (Tree n) => Functor (Tree (S n)) where
  fmap f (B ts) = B ((fmap.fmap) f ts)
  {-# INLINABLE fmap #-}

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

-- TODO: Monad

instance Foldable (Tree Z) where
  foldMap f (L a) = f a
  {-# INLINABLE foldMap #-}

instance Foldable (Tree n) => Foldable (Tree (S n)) where
  foldMap f (B ts) = (foldMap.foldMap) f ts
  {-# INLINABLE foldMap #-}

instance Traversable (Tree Z) where
  traverse f (L a ) = L <$> f a
  {-# INLINABLE traverse #-}

instance Traversable (Tree n) => Traversable (Tree (S n)) where
  traverse f (B ts) = B <$> (traverse.traverse) f ts
  {-# INLINE traverse #-}
