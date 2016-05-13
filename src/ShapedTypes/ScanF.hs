{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.ScanF
-- Copyright   :  (c) 2016 Conal Elliott
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Parallel scan. Experimental version to replace ShapedTypes.Scan
----------------------------------------------------------------------

module ShapedTypes.ScanF
  ( And1, pattern And1, unAnd1, LScanTy, LScan(..), LFScan
  , lscanTraversable, lscanAla
  , lsums, lproducts, lAlls, lAnys, lParities, iota
  , genericLscan
  -- , lscanInc, lsums', lproducts', scanlT, scanlTEx
  ) where

import Prelude hiding (zip,zipWith,unzip)

import Data.Monoid ((<>),Sum(..),Product(..),All(..),Any(..))
import Control.Arrow ((***),first)
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import GHC.Generics

import Control.Newtype (Newtype(..))

import Data.Key

import Circat.Misc ((:*),Parity(..))
import ShapedTypes.Misc (underF)

{--------------------------------------------------------------------
    LScan class
--------------------------------------------------------------------}

type And1 f = f :*: Par1

pattern And1 :: f a -> a -> And1 f a
pattern And1 fa a = fa :*: Par1 a

andTot :: f a :* a -> And1 f a
andTot = uncurry And1
-- andTot (fa,a) = fa :*: Par1 a

unAnd1 :: And1 f a -> f a :* a
unAnd1 (fa :*: Par1 a) = (fa,a)

-- unAnd1 (And1 fa a) = (fa,a)
--
--     Pattern match(es) are non-exhaustive
--     In an equation for ‘unAnd1’: Patterns not matched: _
-- 
-- GHC 8.1.20160405 bug?

type LScanTy f = forall a. Monoid a => f a -> And1 f a

class LScan f where
  lscan :: LScanTy f
  -- Temporary hack to avoid newtype-like representation. Still needed?
  lscanDummy :: f a
  lscanDummy = undefined

-- TODO: Try removing lscanDummy and the comment and recompiling with reification

-- | Traversable version (sequential)
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> And1 t b
scanlT op e = andTot . swap . mapAccumL (\ a b -> (a `op` b,a)) e
{-# INLINABLE scanlT #-}

lscanTraversable :: Traversable t => LScanTy t
lscanTraversable = scanlT mappend mempty
{-# INLINABLE lscanTraversable #-}

type LFScan f = (Functor f, LScan f)

adjustl :: (Monoid a, Functor t) => a -> t a -> t a
-- adjustl p = fmap (p <>)
adjustl = fmap . mappend

{--------------------------------------------------------------------
    Monoid specializations
--------------------------------------------------------------------}

-- Left-scan via a 'Newtype'
lscanAla :: forall n o f. (Newtype n, o ~ O n, LFScan f, Monoid n)
         => (o -> n) -> f o -> And1 f o
lscanAla = flip underF lscan

-- lscanAla k = underF k lscan
-- lscanAla _k = fmap unpack . lscan . fmap (pack :: o -> n)

-- lsums :: (LFScan f, Num b) => f b -> And1 f b
lsums :: (LFScan f, Num b) => f b -> (f :*: Par1) b
lsums = lscanAla Sum

lproducts :: (LFScan f, Num b) => f b -> And1 f b
lproducts = lscanAla Product

lAlls :: LFScan f => f Bool -> And1 f Bool
lAlls = lscanAla All

lAnys :: LFScan f => f Bool -> And1 f Bool
lAnys = lscanAla Any

lParities :: LFScan f => f Bool -> And1 f Bool
lParities = lscanAla Parity

-- | Numbers from 0 to n (size of f). Named for APL iota operation (but 0 based).
iota' :: (LScan f, Traversable f, Applicative f, Num b) => And1 f b
iota' = lsums (pure 1)

-- | Numbers from 0 to n-1. Named for APL iota operation (but 0 based).
iota :: (LScan f, Traversable f, Applicative f, Num b) => f b
iota = fst (unAnd1 iota')

{--------------------------------------------------------------------
    Generic support
--------------------------------------------------------------------}

instance LScan V1 where lscan = \ case

lscanEmpty :: LScanTy f
lscanEmpty fa = And1 fa mempty

instance LScan U1       where lscan = lscanEmpty
instance LScan (K1 i c) where lscan = lscanEmpty

instance LScan Par1 where
  lscan (Par1 a) = And1 (Par1 mempty) a

firstAnd1 :: (f a -> g a) -> And1 f a -> And1 g a
firstAnd1 q (fa :*: Par1 a) = And1 (q fa) a

-- firstAnd1 (And1 fa a) q = And1 (q fa) a  -- non-exhaustive (GHC bug?)

either1 :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
either1 fab _ (L1 fa) = fab fa
either1 _ gab (R1 ga) = gab ga

instance (LScan f, LScan g) => LScan (f :+: g) where
  lscan = either1 (firstAnd1 L1 . lscan) (firstAnd1 R1 . lscan)

--   lscan = either1 (foo L1) (foo R1)
--    where
--      foo k = firstAnd1 k . lscan

-- instance (LScan f, LScan g) => LScan (f :+: g) where
--   lscan (L1 fa) = firstAnd1 L1 (lscan fa)
--   lscan (R1 ga) = firstAnd1 R1 (lscan ga)

instance (LScan f, LFScan g) => LScan (f :*: g) where
  lscan (fa :*: ga) = And1 (fa' :*: ga') gx
   where
     And1 fa' fx = lscan fa
     And1 ga' gx = adjustl fx (lscan ga)
  {-# INLINABLE lscan #-}

instance (LScan g, LFScan f, Zip g) => LScan (g :.: f) where
  lscan (Comp1 gfa) = And1 (Comp1 (zipWith adjustl tots' gfa')) tot
   where
     (gfa', tots)     = unzipAnd1 (lscan <$> gfa)
     And1 tots' tot = lscan tots
  {-# INLINABLE lscan #-}

unzipAnd1 :: forall g f a. Functor g => g (And1 f a) -> g (f a) :* g a
unzipAnd1 = unzip . fmap unAnd1

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip ps = (fst <$> ps, snd <$> ps)

instance LScan f => LScan (M1 i c f) where
  lscan = firstAnd1 M1 . lscan . unM1

-- | Generic left scan
genericLscan :: (Generic1 f, LScan (Rep1 f)) => LScanTy f
genericLscan = firstAnd1 to1 . lscan . from1
