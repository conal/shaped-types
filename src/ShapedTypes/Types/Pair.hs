{-# LANGUAGE CPP                #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DeriveDataTypeable #-}

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
-- Module      :  ShapedTypes.Types.Pair
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Pair representation
----------------------------------------------------------------------

module ShapedTypes.Types.Pair (Pair(..)) where

import Prelude hiding (id,(.))
import Control.Category (id,(.))

-- import Data.Typeable (Typeable)
-- import Data.Data (Data)

-- import Circat.Rep

import Circat.Category (Uncurriable(..),twiceP,(***),(&&&),second,ProductCat(..))
import Circat.Classes (BottomCat(..),IfCat(..),IfT)
import Circat.Circuit
import Circat.Misc ((:*))
#include "Circat/AbsTy.inc"

import Circat.Rep (HasRep(..))

infixl 1 :#
-- | Uniform pairs
data Pair a = a :# a
  deriving (Show)  -- ,Typeable,Data

--   deriving (Functor,Traversable,Eq,Show,Typeable,Data,Generic,Generic1)

instance HasRep (Pair a) where
  type Rep (Pair a) = (a,a)
  repr (a :# a') = (a,a')
  abst (a,a') = (a :# a')

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
-- binary trees.

instance BottomCat (:>) a => BottomCat (:>) (Pair a) where
  bottomC = abstC . (bc &&& bc)
   where
     bc :: () :> a
     bc = bottomC
     {-# NOINLINE bc #-}

instance IfCat (:>) a => IfCat (:>) (Pair a)
 where
   ifC = abstC . pairIf . second (twiceP reprC)

-- Specialization of prodPair
pairIf :: forall k a. (ProductCat k, IfCat k a) => IfT k (a :* a)
pairIf = half exl &&& half exr
  where
    half :: (u `k` a) -> ((Bool :* (u :* u)) `k` a)
    half f = ifC . second (twiceP f)
    {-# NOINLINE half #-}
