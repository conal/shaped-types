{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}

#include "Circat/AbsTy.inc"

AbsTyPragmas

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Types.Vec
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Length-typed lists/vectors
----------------------------------------------------------------------

module ShapedTypes.Types.Vec (Vec(..)) where

import Prelude hiding (id,(.))

import Circat.Rep

import ShapedTypes.Nat

infixr 5 :<

-- | Vectors with type-determined length, having empty vector ('ZVec') and
-- vector cons ('(:<)').
data Vec :: Nat -> * -> * where
  ZVec :: Vec Z a 
  (:<) :: a -> Vec n a -> Vec (S n) a
-- deriving Typeable

instance HasRep (Vec Z a) where
  type Rep (Vec Z a) = ()
  repr ZVec = ()
  abst () = ZVec

instance HasRep (Vec (S n) a) where
  type Rep (Vec (S n) a) = (a,Vec n a)
  repr (a :< as) = (a, as)
  abst (a, as) = (a :< as)
