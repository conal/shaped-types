{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Pair
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some data types for specializing
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin=ReificationRules.Plugin -dcore-lint -fexpose-all-unfoldings #-}

-- {-# OPTIONS_GHC -fplugin-opt=ReificationRules.Plugin:trace #-}
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

module ShapedTypes.Pair (Pair(..),pswap) where

import Data.Monoid ((<>))
import Control.Applicative (liftA2)
-- import Data.Typeable (Typeable)
-- import Data.Data (Data)
-- import GHC.Generics (Generic,Generic1)
import Test.QuickCheck (Arbitrary(..),CoArbitrary(..))

import Data.Key

import ShapedTypes.ApproxEq
import ShapedTypes.Sized
import ShapedTypes.Scan

-- Type definition
import ShapedTypes.Types.Pair

{--------------------------------------------------------------------
    Standard type class instances
--------------------------------------------------------------------}

deriving instance Functor Pair
deriving instance Traversable Pair
deriving instance Eq a => Eq (Pair a)

-- deriving instance Generic1 Pair
-- deriving instance Generic (Pair a)

instance Monoid a => Monoid (Pair a) where
  mempty  = pure   mempty
  mappend = liftA2 mappend
  {-# INLINABLE mempty  #-}
  {-# INLINABLE mappend #-}

-- The derived foldMap inserts a mempty (in GHC 7.0.4).
instance Foldable Pair where
  foldMap f (a :# b) = f a `mappend` f b
  {-# INLINABLE foldMap #-}

instance Applicative Pair where
  pure a = a :# a
  (f :# g) <*> (a :# b) = (f a :# g b)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance Monad Pair where
  return = pure
  m >>= f = joinP (f <$> m)
  {-# INLINABLE return #-}
  {-# INLINABLE (>>=) #-}

joinP :: Pair (Pair a) -> Pair a
joinP ((a :# _) :# (_ :# d)) = a :# d
{-# INLINABLE joinP #-}

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 (:#) arbitrary arbitrary
  shrink (x :# y) = [ x' :# y'  | (x',y') <- shrink (x,y) ]

instance CoArbitrary a => CoArbitrary (Pair a) where
  coarbitrary (x :# y) = coarbitrary x . coarbitrary y

{--------------------------------------------------------------------
    keys package
--------------------------------------------------------------------}

type instance Key Pair = Bool

instance Keyed Pair where
  mapWithKey q = \ (a :# b) -> q False a :# q True b

instance Zip Pair where zipWith = liftA2

instance ZipWithKey Pair

instance Lookup Pair where lookup k t = Just (index t k)

instance Indexable Pair where
  index (a :# b) k = if k then b else a

instance Adjustable Pair where
  adjust f k (a :# b) = if k then a :# f b else f a :# b

{--------------------------------------------------------------------
    shaped-types instances
--------------------------------------------------------------------}

instance ApproxEq a => ApproxEq (Pair a) where (=~) = approxEqFoldable

instance Sized Pair where
  size = 2
  {-# INLINE size #-}

instance LScan Pair where
  lscan (a :# b) = (mempty :# a, a <> b)
  {-# INLINE lscan #-}

{--------------------------------------------------------------------
    Miscellany
--------------------------------------------------------------------}

pswap :: Pair a -> Pair a
pswap (a :# b) = b :# a
