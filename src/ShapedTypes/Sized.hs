{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Sized
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Statically sized functors
----------------------------------------------------------------------

-- TODO: Reconsider whether I can use 'length' from 'Foldable' rather than the
-- Sized type class. Can 'Foldable.length' operate efficiently on our data types
-- (without traversing)?

module ShapedTypes.Sized (Sized(..),genericSize,sizeAF) where

-- TODO: explicit exports

import GHC.Generics

class Sized f where
  size :: f () -> Int -- ^ Argument is ignored at runtime
  -- Temporary hack to avoid newtype-like representation.
  sizeDummy :: f a
  sizeDummy = undefined

-- TODO: Switch from f () to f Void or Proxy

-- | Generic 'size'
genericSize :: (Generic1 f, Sized (Rep1 f)) => f () -> Int
genericSize = size . from1
{-# INLINABLE genericSize #-}

-- The argument to size is unfortunate. When GHC Haskell has explicit type
-- application (<https://ghc.haskell.org/trac/ghc/wiki/TypeApplication>),
-- replace "size (undefined :: f ())" with "size @f".
-- Meanwhile, a macro helps.

#define tySize(f) (size (undefined :: (f) ()))

-- | Useful default for 'size'.
sizeAF :: forall f. (Applicative f, Foldable f) => f () -> Int
sizeAF = const (sum (pure 1 :: f Int))

instance Sized Par1 where
  size = const 1
  {-# INLINABLE size #-}

instance (Sized g, Sized f) => Sized (g :.: f) where
  size = const (tySize(g) * tySize(f))
  {-# INLINABLE size #-}

instance (Sized g, Sized f) => Sized (f :*: g) where
  size = const (tySize(g) + tySize(f))
  {-# INLINABLE size #-}
