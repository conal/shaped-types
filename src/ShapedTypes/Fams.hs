{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Fams
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Vectors and functor exponentiations via type families.
----------------------------------------------------------------------

module ShapedTypes.Fams where

import GHC.Generics hiding (S)

import ShapedTypes.Nat

-- | Right-associated vector
type family RVec n where
  RVec Z     = U1
  RVec (S n) = Par1 :*: RVec n

-- | Left-associated vector
type family LVec n where
  LVec Z     = U1
  LVec (S n) = LVec n :*: Par1


-- | Right-associated functor exponentiation
type family RPow h n where
  RPow h Z     = Par1
  RPow h (S n) = h :.: RPow h n

-- | Left-associated functor exponentiation
type family LPow h n where
  LPow h Z     = Par1
  LPow h (S n) = LPow h n :.: h

-- | Uniform pair
type UPair = Par1 :*: Par1

-- | Uniform pair, alternative formulation (right)
type RPair = RVec N2

-- | Uniform pair, alternative formulation (left)
type LPair = LVec N2
