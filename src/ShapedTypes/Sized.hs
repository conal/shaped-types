{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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

class Sized (f :: * -> *) where size :: Int

-- TODO: Switch from f () to f Void or Proxy

-- | Generic 'size'
genericSize :: forall f. Sized (Rep1 f) => Int
genericSize = size @(Rep1 f)
{-# INLINABLE genericSize #-}

-- The argument to size is unfortunate. When GHC Haskell has explicit type
-- application (<https://ghc.haskell.org/trac/ghc/wiki/TypeApplication>),
-- replace "size (undefined :: f ())" with "size @f".
-- Meanwhile, a macro helps.

-- | Default for 'size' on an applicative functor.
-- Warning: runs in linear time (though possibly at compile time).
sizeAF :: forall f. (Applicative f, Foldable f) => Int
sizeAF = sum (pure 1 :: f Int)

instance Sized Par1 where
  size = 1
  {-# INLINABLE size #-}

instance (Sized g, Sized f) => Sized (g :.: f) where
  size = size @g * size @f
  {-# INLINABLE size #-}

instance (Sized g, Sized f) => Sized (f :*: g) where
  size = size @g + size @f
  {-# INLINABLE size #-}
