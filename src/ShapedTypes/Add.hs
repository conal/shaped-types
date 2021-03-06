{-# LANGUAGE CPP #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#include "Circat/AbsTy.inc"

AbsTyPragmas

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP

-- | Adders

module ShapedTypes.Add where

import Prelude hiding (zipWith)
import Data.Tuple (swap)
import Control.Arrow (first)
import GHC.Generics ((:*:)(..),Par1(..))

import Control.Monad.State (MonadState(..),runState) -- mtl

import Data.Key (Zip(..))
import Control.Newtype

import Circat.Rep
import Circat.Misc ((:*),Binop)  -- ,xor
import Circat.Category (Uncurriable(..), OkayArr)
import Circat.Classes
import Circat.Circuit

import ShapedTypes.Misc ((<*$>))
import ShapedTypes.Pair
import ShapedTypes.Scan
import ShapedTypes.Shift (accumL)
import ShapedTypes.Orphans () -- for Newtype ((f :*: g) a)

type Adder  t =         t (Pair Bool) -> t Bool :* Bool
type Adder' t = Bool :* t (Pair Bool) -> t Bool :* Bool

-- TODO: generalize from single-bit addends, while keeping the single-bit carry
-- in and out.

-- TODO: generalize from pairs and single-bit carry, to perhaps tries and
-- key-valued carries.

{--------------------------------------------------------------------
    One-bit adders
--------------------------------------------------------------------}

data PropGen = PropGen { pgProp :: Bool, pgGen :: Bool }

instance HasRep PropGen where
  type Rep PropGen = Bool :* Bool
  repr (PropGen p g) = (p,g)
  abst (p,g) = PropGen p g

AbsTy(PropGen)

-- MSB on left
instance Monoid PropGen where
  mempty = PropGen True False
  PropGen pa ga `mappend` PropGen pb gb =
    PropGen (pa && pb) ((ga && pb) || gb)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

xor :: Binop Bool
xor = (/=)

propGen :: Pair Bool -> PropGen
propGen = abst . halfAdd
-- propGen (a :# b) = PropGen (a `xor` b) (a && b)
-- {-# INLINE propGen #-}

halfAdd :: Pair Bool -> Bool :* Bool
halfAdd (a :# b) = (a `xor` b,a && b)
{-# INLINE halfAdd #-}

add1 :: Bool :* Pair Bool -> Bool :* Bool
add1 (ci, a :# b) = (s,co)
 where
   q  = a `xor` b
   s  = q `xor` ci
   co = (a && b) || (ci && q)
{-# INLINE add1 #-}

-- Equivalently,
add1' :: Bool :* Pair Bool -> Bool :* Bool
add1' (ci, ab) = (s',co || co')
 where
   (s ,co ) = halfAdd ab
   (s',co') = halfAdd (s :# ci)
{-# INLINE add1' #-}

-- TODO: try having all of these one-bit adders yield a GenProp (or PropGen).
-- Then does add1/add1' use mappend? If this refactoring works, it'll make for
-- an easier time explaining the scan method. I bet that my add1 is equivalent
-- to the trick that scanAdd''' uses for introducing a carry-in bit.

-- TODO: See about generalizing from single-bit additions

{--------------------------------------------------------------------
    mapM
--------------------------------------------------------------------}

add1State :: MonadState Bool m => Pair Bool -> m Bool
add1State p = state (flip (curry add1) p)
{-# INLINE add1State #-}

adderSt :: (MonadState Bool m, Traversable t) =>
           (m (t Bool) -> Bool -> (t Bool, Bool)) -> Adder' t
adderSt run (ci,ps) = run (mapM add1State ps) ci
{-# INLINE adderSt #-}

adderState :: Traversable t => Adder' t
adderState = adderSt runState
{-# INLINE adderState #-}

-- adderStateTrie :: Traversable t => Adder' t
-- adderStateTrie = adderSt runStateTrie
-- {-# INLINE adderStateTrie #-}

{--------------------------------------------------------------------
    accum-based
--------------------------------------------------------------------}

adderAccumL :: Traversable t => Adder' t
adderAccumL = accumL add1
{-# INLINE adderAccumL #-}

-- Operationally (and denotationally) equivalent to adderState, unsurprisingly,
-- since they both use State.

-- accumL :: Traversable t => (a :* b -> c :* a) -> (a :* t b -> t c :* a)

{--------------------------------------------------------------------
    Scan-based
--------------------------------------------------------------------}

-- pgCarry :: GenProp -> Bool -> Bool
-- pgCarry (GenProp g p) cin = g || p && cin -- TODO: consolidate with mappend
-- -- {-# INLINE pgCarry #-}

scanAdd :: (LScan t, Zip t) => Adder t
scanAdd ps = (zipWith h pgs cs, co)
 where
   pgs = propGen <$> ps
   (cs,co) = pgGen <*$> lscan pgs
   h (PropGen p _) ci = p `xor` ci
-- {-# INLINE scanAdd #-}

scanAdd' :: (Zip t, LScan t) => Adder' t
scanAdd' (ci0,ps) = first (snd . unpack) $ scanAdd (Par1 (ci0 :# True) :*: ps)
-- {-# INLINE scanAdd' #-}
