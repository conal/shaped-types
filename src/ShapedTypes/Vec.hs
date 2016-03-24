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
{-# LANGUAGE TypeApplications    #-}
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

module ShapedTypes.Vec (Vec(..)) where

import Prelude hiding (id,(.))
import Circat.Category (id,(.),(***),(&&&),second,twiceP,exl,exr)
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import GHC.Generics (Generic1(..),U1(..),Par1(..),(:*:)(..))

import Data.Key

import Circat.Classes (unitIf)
import Circat.Rep
import Circat.Misc (Unit,(:*))
import Circat.ApproxEq

import ShapedTypes.Sized
import ShapedTypes.Nat
import ShapedTypes.Scan (LScan(..),lscanTraversable)

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
  {-# INLINE fmap #-}

instance Functor (Vec n) => Functor (Vec (S n)) where
  fmap f (a :< u) = f a :< fmap f u
  {-# INLINE fmap #-}
  SPECS(Functor)

instance Applicative (Vec Z) where
  pure _ = ZVec
  ZVec <*> ZVec = ZVec
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure a = a :< pure a
  (f :< fs) <*> (a :< as) = f a :< (fs <*> as)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}
  SPECS(Applicative)

-- TODO: Monad

instance Foldable (Vec Z) where
  foldMap _ ZVec = mempty
  {-# INLINE foldMap #-}

instance Foldable (Vec n) => Foldable (Vec (S n)) where
  foldMap h (a :< as) = h a <> foldMap h as
  {-# INLINE foldMap #-}
  SPECS(Foldable)

instance Traversable (Vec Z) where
  traverse _ ZVec = pure ZVec
  {-# INLINE traverse #-}

instance Traversable (Vec n) => Traversable (Vec (S n)) where
  traverse f (a :< as) = liftA2 (:<) (f a) (traverse f as)
  {-# INLINE traverse #-}
  SPECS(Traversable)

{--------------------------------------------------------------------
   Other representations
--------------------------------------------------------------------}

instance Generic1 (Vec Z) where
  type Rep1 (Vec Z) = U1
  from1 ZVec = U1
  to1 U1 = ZVec

instance Generic1 (Vec (S n)) where
  type Rep1 (Vec (S n)) = Par1 :*: Vec n
  from1 (a :< as) = Par1 a :*: as
  to1 (Par1 a :*: as) = a :< as

instance HasRep (Vec Z a) where
  type Rep (Vec Z a) = ()
  repr ZVec = ()
  abst () = ZVec

instance HasRep (Vec (S n) a) where
  type Rep (Vec (S n) a) = (a,Vec n a)
  repr (a :< as) = (a, as)
  abst (a, as) = (a :< as)

{--------------------------------------------------------------------
    keys package
--------------------------------------------------------------------}

instance (Functor (Vec n), Applicative (Vec n)) => Zip (Vec n) where
  zipWith = liftA2

-- Without the seemingly redundant Functor (Vec n) constraint, GHC 8.1.20160307 says
-- 
--     • Could not deduce (Functor (Vec n))
--         arising from the superclasses of an instance declaration
--       from the context: Applicative (Vec n)
--         bound by the instance declaration
--         at ShapedTypes/Vec.hs:(154,10)-(155,13)
--
-- Perhaps <https://ghc.haskell.org/trac/ghc/ticket/11427>.

#if 0
type instance Key (Vec n) = Fin n

instance Keyed Pair where
  mapWithKey q = \ (a :# b) -> q False a :# q True b

instance Lookup Pair where lookup k t = Just (index t k)

instance Indexable Pair where
  index (a :# b) k = if k then b else a

instance Adjustable Pair where
  adjust f k (a :# b) = if k then a :# f b else f a :# b

instance ZipWithKey (Vec n)
#endif

{--------------------------------------------------------------------
    shaped-types instances
--------------------------------------------------------------------}

instance (Foldable (Vec n), ApproxEq a) => ApproxEq (Vec n a) where
  (=~) = approxEqFoldable
  {-# INLINE (=~) #-}

-- instance (Foldable (Vec n), Applicative (Vec n)) => Sized (Vec n) where
--   size = sizeAF @(Vec n)
--   -- size = length (pure () :: Vec n ())
--   {-# INLINE size #-}

-- instance Sized (Rep1 (Vec n)) => Sized (Vec n) where
--   size = genericSize @(Vec n)
--   {-# INLINE size #-}

instance                  Sized (Vec   Z  ) where
  size = 0
  -- {-# INLINE size #-}
instance Sized (Vec n) => Sized (Vec (S n)) where
  size = 1 + size @(Vec n)
  -- {-# INLINE size #-}

-- Note the *absence* of `INLINE` pragmas, particularly for `S n`. Consequently,
-- the `1 +` gets optimized into unboxed terms, defeating my reifier and giving
-- GHC more opportunity for compile-time simplification. Seems a fragile hack.
-- Find robust ways to let GHC do more simplification.

-- Generic lscan is terrible for Vec, so scan sequentially.
instance Traversable (Vec n) => LScan (Vec n) where
  lscan = lscanTraversable
  {-# INLINE lscan #-}

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
