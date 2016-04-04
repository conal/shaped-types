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
-- Module      :  ShapedTypes.Types.RPow
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Right-associated functor exponentiation
----------------------------------------------------------------------


module ShapedTypes.Types.RPow where

AbsTyImports

import Circat.Rep

import ShapedTypes.Nat

{--------------------------------------------------------------------
    Type and basic manipulation
--------------------------------------------------------------------}

-- Top-down, depth-typed, perfect, binary, leaf trees
data RPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> RPow h Z a
  B :: h (RPow h n a) -> RPow h (S n) a

-- I use "RPow" instead of "Pow" to make compiler output easier to follow.

instance HasRep (RPow h Z a) where
  type Rep (RPow h Z a) = a
  repr (L a) = a
  abst = L

instance HasRep (RPow h (S n) a) where
  type Rep (RPow h (S n) a) = h (RPow h n a)
  repr (B ts) = ts
  abst = B

-- Circuit support
AbsTy(RPow h   Z   a)
AbsTy(RPow h (S n) a)
