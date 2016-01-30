{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

----------------------------------------------------------------------
-- |
-- Module      :  Tests
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Experiments. Compile with "ghc -O Tests", and examine Tests.dump-simpl
--
----------------------------------------------------------------------

module Tests (foo) where

-- TODO: explicit exports

import Control.Applicative (liftA2)

import TypeUnary.TyNat

import ShapedTypes.Pair
import ShapedTypes.Vec
import ShapedTypes.RTree

type Unop  a = a -> a
type Binop a = a -> Unop a

-- foo :: Num a => Unop (Tree (S (S Z)) a)
-- foo = fmap (+1)

-- foo :: Num a => Unop (Tree (S (S (S (S (S (S (S (S (S (S (S Z))))))))))) a)

foo :: Unop (Vec N2 Bool)
foo = fmap not
-- foo x = fmap not x

-- foo = pure True :: Vec N3 Bool

-- foo = liftA2 (+) :: Binop (Pair Int)

-- foo = liftA2 (+) :: Binop (Vec N1 Int)

-- foo = sequenceA :: Pair (Tree N1 Int) -> Tree N1 (Pair Int)
