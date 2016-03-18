-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Linear
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Linear algebra
----------------------------------------------------------------------

module ShapedTypes.Linear where

-- TODO: explicit exports

import Circat.Misc (transpose)

import ShapedTypes.Pair (Pair(..))

dotG :: (Traversable g, Applicative f, Foldable f, Num a) => g (f a) -> a
dotG = sum . fmap product . transpose
{-# INLINE dotG #-}

-- Infix binary dot product
infixl 7 <.>
(<.>) :: (Foldable f, Applicative f, Num a) => f a -> f a -> a
u <.> v = dotG (u :# v)
{-# INLINE (<.>) #-}

infixr 1 $@
infixl 9 .@

-- | Apply a linear transformation represented as a generalized matrix
($@) :: (Foldable m, Applicative m, Functor n, Num a) =>
        n (m a) -> m a -> n a
mat $@ vec = (<.> vec) <$> mat
{-# INLINE ($@) #-}

-- | Compose linear transformations represented as generalized matrices
(.@) :: ( Applicative o, Traversable n, Applicative n
        , Traversable m, Applicative m, Num a ) =>
        o (n a) -> n (m a) -> o (m a)
no .@ mn = transpose ((no $@) <$> transpose mn)
{-# INLINE (.@) #-}

-- no .@ mn = (\ n -> (n <.>) <$> transpose mn) <$> no
