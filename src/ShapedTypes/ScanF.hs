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
  ( LScanTy, LScan(..), LFScan
  , lscanTraversable
  , lsums, lproducts, lAlls, lAnys, lParities, iota
  , genericLscan
  -- , lscanInc, lsums', lproducts', scanlT, scanlTEx
  ) where

import Prelude hiding (zip,zipWith)

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

type AndTot f = f :*: Par1

pattern AndTot :: f a -> a -> AndTot f a
pattern AndTot fa a = fa :*: Par1 a

andTot :: f a :* a -> AndTot f a
andTot = uncurry AndTot
-- andTot (fa,a) = fa :*: Par1 a

unAndTot :: AndTot f a -> f a :* a
unAndTot (fa :*: Par1 a) = (fa,a)

-- unAndTot (AndTot fa a) = (fa,a)
--
--     Pattern match(es) are non-exhaustive
--     In an equation for ‘unAndTot’: Patterns not matched: _
-- 
-- GHC 8.1.20160405 bug?

type LScanTy f = forall a. Monoid a => f a -> AndTot f a

class LScan f where
  lscan :: LScanTy f
  -- Temporary hack to avoid newtype-like representation. Still needed?
  lscanDummy :: f a
  lscanDummy = undefined

-- TODO: Try removing lscanDummy and the comment and recompiling with reification

-- | Traversable version (sequential)
scanlT :: Traversable t => (b -> a -> b) -> b -> t a -> AndTot t b
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
         => (o -> n) -> f o -> AndTot f o
lscanAla = flip underF lscan

-- lscanAla k = underF k lscan
-- lscanAla _k = fmap unpack . lscan . fmap (pack :: o -> n)

lsums :: (LFScan f, Num b) => f b -> AndTot f b
lsums = lscanAla Sum

lproducts :: (LFScan f, Num b) => f b -> AndTot f b
lproducts = lscanAla Product

lAlls :: LFScan f => f Bool -> AndTot f Bool
lAlls = lscanAla All

lAnys :: LFScan f => f Bool -> AndTot f Bool
lAnys = lscanAla Any

lParities :: LFScan f => f Bool -> AndTot f Bool
lParities = lscanAla Parity

-- | Numbers from 0 to n (size of f). Named for APL iota operation (but 0 based).
iota' :: (LScan f, Traversable f, Applicative f, Num b) => AndTot f b
iota' = lsums (pure 1)

-- | Numbers from 0 to n-1. Named for APL iota operation (but 0 based).
iota :: (LScan f, Traversable f, Applicative f, Num b) => f b
iota = fst (unAndTot iota')

{--------------------------------------------------------------------
    Generic support
--------------------------------------------------------------------}

instance LScan V1 where lscan = \ case

lscanEmpty :: LScanTy f
lscanEmpty fa = AndTot fa mempty

instance LScan U1       where lscan = lscanEmpty
instance LScan (K1 i c) where lscan = lscanEmpty

instance LScan Par1 where
  lscan (Par1 a) = AndTot (Par1 mempty) a

firstAndTot :: (f a -> g a) -> AndTot f a -> AndTot g a
firstAndTot q (fa :*: Par1 a) = AndTot (q fa) a

-- firstAndTot (AndTot fa a) q = AndTot (q fa) a  -- non-exhaustive (GHC bug?)

either1 :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
either1 fab _ (L1 fa) = fab fa
either1 _ gab (R1 ga) = gab ga

instance (LScan f, LScan g) => LScan (f :+: g) where
  lscan = either1 (firstAndTot L1 . lscan) (firstAndTot R1 . lscan)

--   lscan = either1 (foo L1) (foo R1)
--    where
--      foo k = firstAndTot k . lscan

-- instance (LScan f, LScan g) => LScan (f :+: g) where
--   lscan (L1 fa) = firstAndTot L1 (lscan fa)
--   lscan (R1 ga) = firstAndTot R1 (lscan ga)

instance (LScan f, LFScan g) => LScan (f :*: g) where
  lscan (fa :*: ga) = AndTot (fa' :*: ga') gx
   where
     AndTot fa' fx = lscan fa
     AndTot ga' gx = adjustl fx (lscan ga)
  {-# INLINABLE lscan #-}

instance (LScan g, LFScan f, Zip g) => LScan (g :.: f) where
  lscan (Comp1 gfa) = AndTot (Comp1 (zipWith adjustl tots' gfa')) tot
   where
     (gfa', tots)   = unzipAndTot (lscan <$> gfa)
     AndTot tots' tot = lscan tots
  {-# INLINABLE lscan #-}

unzipAndTot :: forall g f a. Functor g => g (AndTot f a) -> g (f a) :* g a
unzipAndTot (fmap unAndTot -> ps) = (fst <$> ps, snd <$> ps)

instance LScan f => LScan (M1 i c f) where
  lscan = firstAndTot M1 . lscan . unM1

-- | Generic left scan
genericLscan :: (Generic1 f, LScan (Rep1 f)) => LScanTy f
genericLscan = firstAndTot to1 . lscan . from1
