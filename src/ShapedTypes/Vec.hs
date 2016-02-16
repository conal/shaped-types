{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

#include "Circat/AbsTy.inc"

AbsTyPragmas

{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Vec
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Length-typed lists/vectors
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin-opt=LambdaCCC.Reify:verbose #-}

module ShapedTypes.Vec (Vec(..)) where

import Prelude hiding (id,(.))
import Circat.Category (id,(.),(***),(&&&),second,twiceP,exl,exr)

import Circat.Classes (unitIf)
import Data.Monoid ((<>))
import Control.Applicative (liftA2)

import Circat.Rep
import Circat.Misc (Unit,(:*))

-- import TypeUnary.TyNat
import ShapedTypes.Nat

AbsTyImports
import Circat.Circuit                   -- TODO: specific imports

#define SPEC(cls,n) {-# SPECIALISE instance cls (Vec n) #-}

#define SPECS(cls) \
--   SPEC(cls,N1); SPEC(cls,N2); SPEC(cls,N3); SPEC(cls,N4);\
--   SPEC(cls,N5); SPEC(cls,N6); SPEC(cls,N7); SPEC(cls,N8)

-- The more specializations we declare here, the more time it takes to compile
-- this library code *and* the less time it takes to compile client code. We
-- thus probably want to comment out all or some of the `SPEC`s in `SPECS` while
-- developing.

infixr 5 :<

-- | Vectors with type-determined length, having empty vector ('ZVec') and
-- vector cons ('(:<)').
data Vec :: Nat -> * -> * where
  ZVec :: Vec Z a 
  (:<) :: a -> Vec n a -> Vec (S n) a
-- deriving Typeable

instance Functor (Vec Z) where
  fmap _ ZVec = ZVec
  {-# INLINABLE fmap #-}

instance Functor (Vec n) => Functor (Vec (S n)) where
  fmap f (a :< u) = f a :< fmap f u
  {-# INLINABLE fmap #-}
  SPECS(Functor)

instance Applicative (Vec Z) where
  pure _ = ZVec
  ZVec <*> ZVec = ZVec
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure a = a :< pure a
  (f :< fs) <*> (a :< as) = f a :< (fs <*> as)
  {-# INLINABLE pure  #-}
  {-# INLINABLE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (Vec Z) where
  foldMap _ ZVec = mempty
  {-# INLINABLE foldMap #-}

instance Foldable (Vec n) => Foldable (Vec (S n)) where
  foldMap h (a :< as) = h a <> foldMap h as
  {-# INLINABLE foldMap #-}
  SPECS(Foldable)

instance Traversable (Vec Z) where
  traverse _ ZVec = pure ZVec
  {-# INLINABLE traverse #-}

instance Traversable (Vec n) => Traversable (Vec (S n)) where
  traverse f (a :< as) = liftA2 (:<) (f a) (traverse f as)
  {-# INLINABLE traverse #-}
  SPECS(Traversable)

type instance Rep (Vec Z a) = ()
instance HasRep (Vec Z a) where
  repr ZVec = ()
  abst () = ZVec

type instance Rep (Vec (S n) a) = (a,Vec n a)
instance HasRep (Vec (S n) a) where
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
