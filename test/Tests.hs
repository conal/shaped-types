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

{-# OPTIONS_GHC -funfolding-use-threshold=0 -ddump-simpl -ddump-to-file -dppr-case-as-let
      -dsuppress-module-prefixes -dsuppress-idinfo -dsuppress-uniques -dsuppress-coercions #-}

-- TODO: Revisit these flags, and perhaps move to Makefile.
-- Do I still want -funfolding-use-threshold=0 ?

module Tests where

-- TODO: explicit exports

import Control.Applicative (liftA2)

import ShapedTypes.Nat
import ShapedTypes.Pair
import ShapedTypes.Vec
import ShapedTypes.RTree

type Unop  a = a -> a
type Binop a = a -> Unop a

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

bar :: Unop (Tree N1 Bool)
bar = fmap not

-- foo :: Num a => Unop (Tree (S (S Z)) a)
-- foo = fmap (+1)

-- foo :: Num a => Unop (Tree (S (S (S (S (S (S (S (S (S (S (S Z))))))))))) a)

-- foo :: Unop a -> Unop (Vec N2 a)
-- foo = fmap

-- foo :: Unop (Tree N2 Bool)
-- foo = fmap not

-- foo = pure True :: Vec N4 Bool

-- foo = liftA2 (+) :: Binop (Tree N1 Int)

-- foo :: Num a => Binop (Tree N1 a)
-- foo = liftA2 (+)

-- foo :: Binop (Tree N3 Int)
-- foo = liftA2 (+)

-- foo :: Tree N3 (Int -> Bool) -> Tree N3 Int -> Tree N3 Bool
-- foo = (<*>)

-- foo :: Binop Bool -> Binop (Tree N4 Bool)
-- foo = liftA2

-- foo :: Binop a -> Binop (Tree N3 a)
-- foo = liftA2

-- foo = sequenceA :: Pair (Tree N6 Int) -> Tree N6 (Pair Int)

-- -- Without SPECIALIZE: 0.66 second, 100 definitions (1118 lines), 32 specialization rules
-- -- SPECIALIZE to 4: 0.5 second, 52 definitions (549 lines), 21 specialization rules
-- -- SPECIALIZE to 8: 0.3 second, 1 definition (11 lines), 0 specialization rules
-- foo = sequenceA :: Tree N7 (Tree N8 Int) -> Tree N8 (Tree N7 Int)

-- -- 1.0 second, 192 Core definitions (2352 lines), 79 specialization rules
-- foo = sequenceA :: Tree N15 (Tree N16 Int) -> Tree N16 (Tree N15 Int)

-- -- 2.6 seconds, 43 Core definitions (884 lines), 24 specialization rules
-- foo = sequenceA :: Vec N15 (Tree N16 Int) -> Tree N16 (Vec N15 Int)

-- -- 21.2 seconds, 1 definition (2149 lines), no specialization rules.
-- foo = sequenceA :: Vec N15 (Vec N16 Int) -> Vec N16 (Vec N15 Int)
