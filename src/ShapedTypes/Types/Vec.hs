{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

#include "Circat/AbsTy.inc"

AbsTyPragmas

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Types.Vec
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Length-typed lists/vectors
----------------------------------------------------------------------

module ShapedTypes.Types.Vec (Vec(..)) where

import Prelude hiding (id,(.))

import Circat.Category (exl,exr,id,second,twiceP,(&&&),(***),(.),Uncurriable(..))
import Circat.Circuit
import Circat.Classes (unitIf)
import Circat.Classes (BottomCat (..),IfCat (..))
import Circat.Misc ((:*),Unit)
import Circat.Rep

#include "Circat/AbsTy.inc"

import ShapedTypes.Nat

infixr 5 :<

-- | Vectors with type-determined length, having empty vector ('ZVec') and
-- vector cons ('(:<)').
data Vec :: Nat -> * -> * where
  ZVec :: Vec Z a 
  (:<) :: a -> Vec n a -> Vec (S n) a
-- deriving Typeable

instance HasRep (Vec Z a) where
  type Rep (Vec Z a) = ()
  repr ZVec = ()
  abst () = ZVec

instance HasRep (Vec (S n) a) where
  type Rep (Vec (S n) a) = (a,Vec n a)
  repr (a :< as) = (a, as)
  abst (a, as) = (a :< as)

{--------------------------------------------------------------------
    Circuit support
--------------------------------------------------------------------}

#if 0
AbsTy(Vec Z a)
AbsTy(Vec (S n) a)

-- TODO: custom AbsTy replacement for Vec, as I'll be using it for n-ary trees.
#else

instance GenBuses q_q => Uncurriable (:>) q_q (Vec n a) where
  uncurries = id

instance (Applicative (Vec n), Traversable (Vec n), GenBuses a) => GenBuses (Vec n a) where
  genBuses' prim ins = buses <$> sequenceA (pure gb)
   where
     gb :: BusesM (Buses a)
     gb = genBuses' prim ins
     {-# NOINLINE gb #-}  -- still necessary?
     buses :: Vec m (Buses a) -> Buses (Vec m a)
     buses ZVec = abstB UnitB
     buses (b :< bs) = abstB (PairB b (buses bs))

  delay :: Vec n a -> (Vec n a :> Vec n a)
  delay = go
   where
     go :: Vec m a -> (Vec m a :> Vec m a)
     go ZVec = id
     go (a :< as) = abstC . (del a *** go as) . reprC
     del :: a -> (a :> a)
     del = delay
     {-# NOINLINE del #-}  -- still necessary?

  ty :: Vec n a -> Ty
  ty = const (foldr PairT UnitT (pure t :: Vec n Ty))
   where
     t = ty (undefined :: a)
     {-# NOINLINE t #-}

instance (Applicative (Vec n), BottomCat (:>) a) => BottomCat (:>) (Vec n a) where
  bottomC :: Unit :> Vec n a
  bottomC = go (pure ())
   where
     go :: Vec m () -> (Unit :> Vec m a)
     go ZVec = abstC
     go (() :< units) = abstC . (bc &&& go units)
     bc :: Unit :> a
     bc = bottomC
     {-# NOINLINE bc #-}

instance (Applicative (Vec n), IfCat (:>) a) => IfCat (:>) (Vec n a)
 where
   ifC :: Bool :* (Vec n a :* Vec n a) :> Vec n a
   ifC = go (pure ())
    where
      go :: Vec m () -> Bool :* (Vec m a :* Vec m a) :> Vec m a
      go ZVec = abstC . unitIf . second (twiceP reprC)
      go (() :< units) = abstC
                       . ((ifc . second (twiceP exl)) &&& (go units . second (twiceP exr)))
                       . second (twiceP reprC)
      ifc :: Bool :* (a :* a) :> a
      ifc = ifC
      {-# NOINLINE ifc #-}

-- Without these NOINLINE pragmas, GHC's typechecker does exponential work for
-- n-ary trees.

#if 0
reprC :: Vec (S m) a :> Rep (Vec (S m) a)
twiceP reprC :: Vec (S m) a :* Vec (S m) a :> Rep (Vec (S m) a) :* Rep (Vec (S m) a)
second (twiceP reprC)
  :: Bool :* (Vec (S m) a :* Vec (S m) a) :> Bool :* (Rep (Vec (S m) a) :* Rep (Vec (S m) a))
  :: Bool :* (Vec (S m) a :* Vec (S m) a) :> Bool :* (a :* Vec m a) :* (a :* Vec m a)

second (twiceP exl) :: Bool :* (a :* Vec m a) :* (a :* Vec m a) :> Bool :* a :* a
ifc . second (twiceP exl) :: Bool :* (a :* Vec m a) :* (a :* Vec m a) :> a

second (twiceP exr)
  :: Bool :* (a :* Vec m a) :* (a :* Vec m a) :> Bool :* (Vec m a :* Vec m a)
go units :: Bool :* (Vec m a :* Vec m a) :> Vec m a
go units . second (twiceP exr) :: Bool :* (a :* Vec m a) :* (a :* Vec m a) :> Vec m a

(ifc . second (twiceP exl)) &&& (go units . second (twiceP exr))
  :: Bool :* (a :* Vec m a) :* (a :* Vec m a) :> a :* Vec m a

abstC . ((ifc . second (twiceP exl)) &&& (go units . second (twiceP exr))) . second (twiceP reprC)
  :: Bool :* (Vec (S m) a) :* (Vec (S m) a) :> Vec (S m) a
#endif

-- TODO: Look for simple formulations

-- Without NOINLINE pragmas, GHC's typechecker does exponential work for
-- trees.
--
-- TODO: Try again without NOINLINE, since I've reworked these definitions.

-- TODO: Abstract these definitions into something reusable.

#endif
