{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Scan
-- Copyright   :  (c) 2016 Conal Elliott
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Parallel scan
----------------------------------------------------------------------

module ShapedTypes.Scan
  ( LScanTy, LScan(..)
  , lscanT, lscanTraversable
  , lsums, lproducts, lAlls, lAnys, lParities
  , multiples, powers, iota
  ) where

import Prelude hiding (zip,unzip,zipWith)

import Data.Monoid ((<>),Sum(..),Product(..),All(..),Any(..))
import Control.Arrow ((***),first)
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import GHC.Generics

import Control.Newtype (Newtype(..))

import Data.Key
import Data.Pointed

import Circat.Misc ((:*),Parity(..)) -- , Unop
-- import ShapedTypes.Misc (mapl)

type LScanTy f = forall a. Monoid a => f a -> f a :* a

class Functor f => LScan f where
  lscan :: LScanTy f
  default lscan :: (Generic1 f, LScan (Rep1 f)) => LScanTy f
  lscan = first to1 . lscan . from1
  -- Temporary hack to avoid newtype-like representation. Still needed?
  lscanDummy :: f a
  lscanDummy = undefined

-- TODO: Try removing lscanDummy and the comment and recompiling with reification

-- | Traversable version (sequential)
lscanT :: Traversable t => (b -> a -> b) -> b -> t a -> (t b,b)
lscanT op e = swap . mapAccumL (\ b a -> (b `op` a,b)) e
{-# INLINABLE lscanT #-}

lscanTraversable :: Traversable t => LScanTy t
lscanTraversable = lscanT mappend mempty
{-# INLINABLE lscanTraversable #-}

{--------------------------------------------------------------------
    Monoid specializations
--------------------------------------------------------------------}

-- Left-scan via a 'Newtype'
lscanAla :: forall n o f. (Newtype n, o ~ O n, LScan f, Monoid n)
         => f o -> f o :* o
lscanAla = (fmap unpack *** unpack) . lscan . fmap (pack @n)

-- lscanAla k = underF k lscan
-- lscanAla _k = fmap unpack . lscan . fmap (pack :: o -> n)

lsums :: forall f a. (LScan f, Num a) => f a -> (f a, a)
lsums = lscanAla @(Sum a)
{-# INLINABLE lsums #-}

lproducts :: forall f a. (LScan f, Num a) => f a -> f a :* a
lproducts = lscanAla @(Product a)
{-# INLINABLE lproducts #-}

lAlls :: LScan f => f Bool -> (f Bool, Bool)
lAlls = lscanAla @All
{-# INLINABLE lAlls #-}

lAnys :: LScan f => f Bool -> (f Bool, Bool)
lAnys = lscanAla @Any
{-# INLINABLE lAnys #-}

lParities :: LScan f => f Bool -> (f Bool, Bool)
lParities = lscanAla @Parity
{-# INLINABLE lParities #-}

multiples :: (LScan f, Pointed f, Num a) => a -> f a :* a
multiples = lsums . point

powers :: (LScan f, Pointed f, Num a) => a -> f a :* a
powers = lproducts . point

-- | Numbers from 0 to n (size of f). Named for APL iota operation (but 0 based).
iota :: (LScan f, Pointed f, Num a) => f a :* a
iota = multiples 1

{--------------------------------------------------------------------
    Generic support
--------------------------------------------------------------------}

instance LScan V1 where
  lscan = \ case

instance LScan U1 where
  lscan U1 = (U1, mempty)

instance LScan (K1 i c) where
  lscan w@(K1 _) = (w, mempty)

instance LScan Par1 where
  lscan (Par1 a) = (Par1 mempty, a)

instance (LScan f, LScan g) => LScan (f :+: g) where
  lscan (L1 fa) = first L1 (lscan fa)
  lscan (R1 ga) = first R1 (lscan ga)

instance (LScan f, LScan g) => LScan (f :*: g) where
  lscan (fa :*: ga) = (fa' :*: ((fx <>) <$> ga'), fx <> gx)
   where
     (fa', fx) = lscan fa
     (ga', gx) = lscan ga

-- Alternatively,

--   lscan (fa :*: ga) = (fa' :*: ga', gx)
--    where
--      (fa', fx) =               lscan fa
--      (ga', gx) = mapl (fx <>) (lscan ga)

instance (LScan g, LScan f, Zip g) =>  LScan (g :.: f) where
  lscan (Comp1 gfa) = (Comp1 (zipWith adjustl tots' gfa'), tot)
   where
     (gfa', tots)  = unzip (lscan <$> gfa)
     (tots',tot)   = lscan tots
     adjustl t     = fmap (t <>)

-- TODO: maybe zipWith (fmap . mappend) tots' gfa'

instance LScan f => LScan (M1 i c f) where
  lscan (M1 as) = first M1 (lscan as)

unzip :: forall f a b. Functor f => f (a :* b) -> f a :* f b
unzip ps = (fst <$> ps, snd <$> ps)
