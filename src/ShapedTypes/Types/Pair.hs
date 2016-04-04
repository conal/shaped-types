{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

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

import Data.Typeable (Typeable)
import Data.Data (Data)

import Circat.Rep (HasRep(..))

infixl 1 :#
-- | Uniform pairs
data Pair a = a :# a
  deriving (Show,Typeable,Data)

--   deriving (Functor,Traversable,Eq,Show,Typeable,Data,Generic,Generic1)

instance HasRep (Pair a) where
  type Rep (Pair a) = (a,a)
  repr (a :# a') = (a,a')
  abst (a,a') = (a :# a')
