{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Pair
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some data types for specializing
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin=ShapedTypes.Plugin -fobject-code #-}

-- {-# OPTIONS_GHC -funfolding-use-threshold=0 -ddump-simpl -ddump-to-file -dppr-case-as-let -dsuppress-module-prefixes -dsuppress-idinfo -dsuppress-uniques -dsuppress-coercions -dverbose-core2core #-}

module ShapedTypes.Pair (Pair(..)) where

-- import Data.Typeable (Typeable)
-- import Data.Data (Data)

import Circat.Rep

infixl 1 :#
-- | Uniform pairs
data Pair a = a :# a deriving (Functor,Traversable) -- ,Eq,Show,Typeable,Data,Generic,Generic1

-- The derived foldMap inserts a mempty (in GHC 7.0.4).
instance Foldable Pair where
  foldMap f (a :# b) = f a `mappend` f b
  {-# INLINE foldMap #-}

instance Applicative Pair where
  pure a = a :# a
  (f :# g) <*> (a :# b) = (f a :# g b)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Pair where
  return = pure
  m >>= f = joinP (f <$> m)
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

joinP :: Pair (Pair a) -> Pair a
joinP ((a :# _) :# (_ :# d)) = a :# d
{-# INLINE joinP #-}


type instance Rep (Pair a) = (a,a)
instance HasRep (Pair a) where
  repr (a :# a') = (a,a')
  abst (a,a') = (a :# a')
