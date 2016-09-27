{-# LANGUAGE CPP  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

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
import qualified ShapedTypes.Pair as P

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

-- data Bush a = NilB | ConsB a (Bush (Bush a))

-- Bushy binary leaf trees
type family Bush n where
  Bush Z     = P.Pair
  Bush (S n) = Bush n :.: Bush n

-- We can generalize from Pair and from squaring.

-- I use P.Pair instead of UPair for FFT-friendliness, although RPair and LPair
-- would also probably do fine.

-- Bushy binary trees with data at branchings
type family Bush' n where
  Bush' Z     = U1
  Bush' (S n) = Par1 :*: (Bush' n :.: Bush' n)

-- I adapted Bush' from *Nested Datatypes* by Richard Bird and Lambert Meertens
-- (1998), <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.184.8120>,
-- adding depth-indexing. It works fine for maps, folds, traversals, and scans.
-- The use of (:*:) thwarts FFT.
