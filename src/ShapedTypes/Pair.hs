{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveFunctor      #-}
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

-- import Data.Typeable (Typeable)
-- import Data.Data (Data)

import Control.Category (id,(.))

import Circat.Rep

import Circat.Category (Uncurriable(..),twiceP,(***),(&&&),second)
import Circat.Classes (BottomCat(..),IfCat(..))
import Circat.Circuit
#include "Circat/AbsTy.inc"

infixl 1 :#
-- | Uniform pairs
data Pair a = a :# a -- deriving (Functor,Traversable) -- ,Eq,Show,Typeable,Data,Generic,Generic1

-- TODO: retry with deriving

type instance Rep (Pair a) = (a,a)
instance HasRep (Pair a) where
  repr (a :# a') = (a,a')
  abst (a,a') = (a :# a')

-- deriving instance Functor     Pair

instance Functor Pair where
  fmap f (a :# b) = f a :# f b
  {-# INLINABLE fmap #-}

deriving instance Traversable Pair

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
