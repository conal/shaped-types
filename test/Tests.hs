{-# LANGUAGE GADTs, DataKinds, FlexibleContexts #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- TEMP

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

-- {-# OPTIONS_GHC  -fplugin=LambdaCCC.Plugin #-}
-- {-# OPTIONS_GHC  -fplugin-opt=LambdaCCC.Reify:verbose #-}

-- {-# OPTIONS_GHC -funfolding-use-threshold=0 -ddump-simpl -ddump-to-file -dppr-case-as-let -dsuppress-module-prefixes -dsuppress-idinfo -dsuppress-coercions -dverbose-core2core #-}

-- TODO: Revisit these flags, and perhaps move to Makefile.
-- Do I still want -funfolding-use-threshold=0 ?

module Tests where

-- TODO: explicit exports

import Control.Applicative (liftA2)

import ShapedTypes.Nat
import ShapedTypes.Pair
import ShapedTypes.Vec
import qualified ShapedTypes.RPow as R
import qualified ShapedTypes.LPow as L
import ShapedTypes.Scan
import ShapedTypes.FFT

import qualified ShapedTypes.Fams as F

type Unop  a = a -> a
type Binop a = a -> Unop a

type RTree = R.Pow Pair
type LTree = L.Pow Pair

-- foo :: Num a => a -> a
-- foo = (+ 3)

-- bar :: (Foldable f, Num a) => f a -> a
-- bar = sum

-- baz :: Binop Bool
-- baz = (&&)

-- vec2I :: Vec N2 Int
-- vec2I = pure 3

-- bar :: Num a => Unop (Tree N1 a)
-- bar = fmap (+1)

-- bar :: Unop (Pair Bool)
-- bar = fmap not

-- bar :: Unop (RTree N4 Bool)
-- bar = fmap not

-- fmapT :: Functor (RTree n) => (a -> b) -> RTree (S n) a -> RTree (S n) b
-- fmapT f (B ts) = B ((fmap.fmap) f ts)

-- foldMapT :: (Foldable (RTree n), Monoid m) => (a -> m) -> RTree (S n) a -> m
-- foldMapT f (B ts) = (foldMap.foldMap) f ts

-- foldMapT1 :: (Foldable (RTree Z), Monoid m) => (a -> m) -> RTree (S Z) a -> m
-- foldMapT1 f (B ts) = (foldMap.foldMap) f ts

-- foo :: Monoid m => (a -> m) -> RTree Z a -> m
-- foo = foldMap

-- foo :: Num a => Unop (RTree (S (S Z)) a)
-- foo = fmap (+1)

-- foo :: Num a => Unop (RTree (S (S (S (S (S (S (S (S (S (S (S Z))))))))))) a)

-- foo :: Unop a -> Unop (Vec N2 a)
-- foo = fmap

-- foo :: Unop (RTree N2 Bool)
-- foo = fmap not

-- foo = pure True :: Vec N4 Bool

-- foo = liftA2 (+) :: Binop (RTree N1 Int)

-- foo :: Num a => Binop (RTree N1 a)
-- foo = liftA2 (+)

-- foo :: Binop (RTree N3 Int)
-- foo = liftA2 (+)

-- foo :: RTree N3 (Int -> Bool) -> RTree N3 Int -> RTree N3 Bool
-- foo = (<*>)

-- foo :: Binop Bool -> Binop (RTree N5 Bool)
-- foo = liftA2

-- foo :: Binop a -> Binop (RTree N3 a)
-- foo = liftA2

foo = sequenceA :: Pair (RTree N6 Int) -> RTree N6 (Pair Int)

-- -- Without SPECIALIZE: 0.66 second, 100 definitions (1118 lines), 32 specialization rules
-- -- SPECIALIZE to 4: 0.5 second, 52 definitions (549 lines), 21 specialization rules
-- -- SPECIALIZE to 8: 0.3 second, 1 definition (11 lines), 0 specialization rules
-- foo = sequenceA :: RTree N7 (RTree N8 Int) -> RTree N8 (RTree N7 Int)

-- -- 1.0 second, 192 Core definitions (2352 lines), 79 specialization rules
-- foo = sequenceA :: RTree N15 (RTree N16 Int) -> RTree N16 (RTree N15 Int)

-- -- 2.6 seconds, 43 Core definitions (884 lines), 24 specialization rules
-- foo = sequenceA :: Vec N15 (RTree N16 Int) -> RTree N16 (Vec N15 Int)

-- -- 21.2 seconds, 1 definition (2149 lines), no specialization rules.
-- foo = sequenceA :: Vec N15 (Vec N16 Int) -> Vec N16 (Vec N15 Int)
