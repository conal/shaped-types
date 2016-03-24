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
{-# LANGUAGE TypeOperators       #-}

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
  ( LScanTy, LScan(..), LFScan
  , lscanTraversable
  , lsums, lproducts, lAlls, lAnys, lParities, iota
  , lscanProd, lscanProd', lscanComp, lscanComp'
  , genericLscan
  -- , lscanInc, lsums', lproducts', scanlT, scanlTEx
  ) where

import Prelude hiding (zip,unzip,zipWith)

import Data.Monoid ((<>),Sum(..),Product(..),All(..),Any(..))
import Control.Arrow ((***),first)
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import GHC.Generics

import Data.Key

import Circat.Misc ((:*),Parity(..)) -- , Unop

type LScanTy f = forall a. Monoid a => f a -> f a :* a

class LScan f where
  lscan :: LScanTy f
  -- Temporary hack to avoid newtype-like representation. Still needed?
  lscanDummy :: f a
  lscanDummy = undefined

-- TODO: Try removing lscanDummy and the comment and recompiling with reification

-- | Traversable version (sequential)
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> (t b,b)
scanlT op e = swap . mapAccumL (\ a b -> (a `op` b,a)) e
{-# INLINABLE scanlT #-}

lscanTraversable :: Traversable t => LScanTy t
lscanTraversable = scanlT mappend mempty
{-# INLINABLE lscanTraversable #-}

type LFScan f = (Functor f, LScan f)

-- | Scan a product of functors. See also 'lscanProd'.
lscanProd' :: (Functor g, Monoid a)
           => LScanTy f -> LScanTy g
           -> f a :* g a -> (f a :* g a) :* a
lscanProd' lscanF lscanG (fa,ga) = ((fa', tweak <$> ga'), tweak gx)
 where
   (fa',fx) = lscanF fa
   (ga',gx) = lscanG ga
   tweak   = (fx <>)

-- | Scan a product of functors. See also 'lscanProd''.
lscanProd :: (Functor g, Monoid a, LScan f, LScan g)
          => (f a :* g a) -> (f a :* g a) :* a
lscanProd = lscanProd' lscan lscan

-- | Variant of 'lscanComp' useful with size-indexed functors
lscanComp' :: (Zip g, Functor f, Monoid a) =>
              LScanTy g -> LScanTy f
           -> g (f a) -> g (f a) :* a
lscanComp' lscanG lscanF gfa  = (zipWith adjustl tots' gfa', tot)
 where
   (gfa' ,tots)  = unzip (lscanF <$> gfa)
   (tots',tot)   = lscanG tots

unzip :: forall f a b. Functor f => f (a :* b) -> f a :* f b
unzip ps = (fst <$> ps, snd <$> ps)

-- unzip ps = (fmapF fst ps, fmapF snd ps)
--  where
--    fmapF :: forall u v. (u -> v) -> f u -> f v
--    fmapF = fmap

adjustl :: (Monoid a, Functor t) => a -> t a -> t a
adjustl p = fmap (p <>)

-- | Scan a composition of functors
lscanComp :: (LScan g, LFScan f, Zip g, Monoid a) =>
             g (f a) -> g (f a) :* a
lscanComp = lscanComp' lscan lscan

lsums :: (LFScan f, Num b) => f b -> (f b, b)
lsums = (fmap getSum *** getSum) . lscan . fmap Sum
{-# INLINABLE lsums #-}

lproducts :: (LFScan f, Num b) => f b -> f b :* b
lproducts = (fmap getProduct *** getProduct) . lscan . fmap Product
{-# INLINABLE lproducts #-}

lAlls :: LFScan f => f Bool -> (f Bool, Bool)
lAlls = (fmap getAll *** getAll) . lscan . fmap All
{-# INLINABLE lAlls #-}

lAnys :: LFScan f => f Bool -> (f Bool, Bool)
lAnys = (fmap getAny *** getAny) . lscan . fmap Any
{-# INLINABLE lAnys #-}

lParities :: LFScan f => f Bool -> (f Bool, Bool)
lParities = (fmap getParity *** getParity) . lscan . fmap Parity
{-# INLINABLE lParities #-}

-- TODO: Refactor lsums, lproducts, etc, using Newtype.

-- Variants 

-- | Numbers from 0 to n-1. Named for APL iota operation (but 0 based).
iota :: (LScan f, Traversable f, Applicative f, Num b) => f b
iota = fst (lsums (pure 1))

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

instance (LScan f, LFScan g) => LScan (f :*: g) where
  lscan (fa :*: ga) = first (uncurry (:*:)) (lscanProd (fa,ga))

instance (LScan g, LFScan f, Zip g) => LScan (g :.: f) where
  lscan (Comp1 gfa) = first Comp1 (lscanComp gfa)

instance LScan f => LScan (M1 i c f) where
  lscan (M1 as) = first M1 (lscan as)

-- | Generic left scan
genericLscan :: (Generic1 f, LScan (Rep1 f)) => LScanTy f
genericLscan = first to1 . lscan . from1
