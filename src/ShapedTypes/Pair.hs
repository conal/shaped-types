{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- For circuit support
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall #-}

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

-- {-# OPTIONS_GHC -fplugin-opt=LambdaCCC.Reify:verbose #-}
-- {-# OPTIONS_GHC -ddump-deriv #-}

module ShapedTypes.Pair (Pair(..)) where

import Prelude hiding (id,(.))
import Data.Monoid ({-Monoid(..),-}(<>))
import Control.Category (id,(.))
import Control.Applicative (liftA2)
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic,Generic1) 
import Test.QuickCheck (Arbitrary(..),CoArbitrary(..))

import Data.Key

import Circat.Rep
import Circat.ApproxEq

import Circat.Category (Uncurriable(..),twiceP,(***),(&&&),second)
import Circat.Classes (BottomCat(..),IfCat(..))
import Circat.Circuit
#include "Circat/AbsTy.inc"

import ShapedTypes.Sized
import ShapedTypes.Scan
import ShapedTypes.FFT

infixl 1 :#
-- | Uniform pairs
data Pair a = a :# a deriving (Functor,Traversable,Eq,Show,Typeable,Data,Generic,Generic1)

-- TODO: retry with deriving

instance HasRep (Pair a) where
  type Rep (Pair a) = (a,a)
  repr (a :# a') = (a,a')
  abst (a,a') = (a :# a')

{--------------------------------------------------------------------
    Standard type class instances
--------------------------------------------------------------------}

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
  size = const 2
  {-# INLINE size #-}

instance LScan Pair where
  lscan (a :# b) = (mempty :# a, a <> b)
  {-# INLINE lscan #-}

-- Radix 2 butterfly
instance FFT Pair Pair where
  fft (a :# b) = (a + b) :# (a - b)
  {-# INLINE fft #-}

{--------------------------------------------------------------------
    Circuit support
--------------------------------------------------------------------}

instance GenBuses q_q => Uncurriable (:>) q_q (Pair a) where
  uncurries = id

instance GenBuses a => GenBuses (Pair a) where
  genBuses' prim ins = abstB <$> (PairB <$> gb <*> gb)
   where
     gb :: BusesM (Buses a)
     gb = genBuses' prim ins
     {-# NOINLINE gb #-}
  delay (a :# b) = abstC . (del a *** del b) . reprC
   where
     del :: a -> (a :> a)
     del = delay
     {-# NOINLINE del #-}
  ty = const (PairT t t)
   where
     t = ty (undefined :: a)
     {-# NOINLINE t #-}

-- Without these NOINLINE pragmas, GHC's typechecker does exponential work for
-- binary trees. I'll want to do something similar for Vec as well so that n-ary
-- trees don't blow up.

instance BottomCat (:>) a => BottomCat (:>) (Pair a) where
  bottomC = abstC . (bc &&& bc)
   where
     bc :: () :> a
     bc = bottomC
     {-# NOINLINE bc #-}

instance IfCat (:>) a => IfCat (:>) (Pair a)
 where
   ifC = abstC . pairIf . second (twiceP reprC)
