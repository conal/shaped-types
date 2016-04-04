{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}

#include "Circat/AbsTy.inc"

AbsTyPragmas

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Types.LPow
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Right-associated functor exponentiation
----------------------------------------------------------------------

module ShapedTypes.Types.LPow where

AbsTyImports

import Circat.Rep

import ShapedTypes.Nat

{--------------------------------------------------------------------
    Type and basic manipulation
--------------------------------------------------------------------}

-- Top-down, depth-typed, perfect, binary, leaf trees
data LPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> LPow h Z a
  B :: LPow h n (h a) -> LPow h (S n) a

-- I use "LPow" instead of "Pow" to make compiler output easier to follow.

instance HasRep (LPow h Z a) where
  type Rep (LPow h Z a) = a
  repr (L a) = a
  abst = L

instance HasRep (LPow h (S n) a) where
  type Rep (LPow h (S n) a) = LPow h n (h a)
  repr (B t) = t
  abst t = B t

-- Circuit support
AbsTy(LPow h   Z   a)
AbsTy(LPow h (S n) a)
