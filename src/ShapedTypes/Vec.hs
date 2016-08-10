{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
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

-- {-# OPTIONS_GHC -fplugin=ReificationRules.Plugin -dcore-lint -fexpose-all-unfoldings #-}

-- {-# OPTIONS_GHC -fplugin-opt=ReificationRules.Plugin:trace #-}
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

module ShapedTypes.Vec (Vec(..)) where

import Prelude hiding (id,(.))
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import GHC.Generics (Generic1(..),U1(..),Par1(..),(:*:)(..))

import Data.Pointed
import Data.Key

import ShapedTypes.ApproxEq
import ShapedTypes.Sized
import ShapedTypes.Nat
import ShapedTypes.Scan (LScan(..),lscanTraversable)

import ShapedTypes.Types.Vec

#define SPEC(cls,n) {-# SPECIALISE instance cls (Vec n) #-}

#define SPECS(cls) \
--   SPEC(cls,N1); SPEC(cls,N2); SPEC(cls,N3); SPEC(cls,N4);\
--   SPEC(cls,N5); SPEC(cls,N6); SPEC(cls,N7); SPEC(cls,N8)

-- The more specializations we declare here, the more time it takes to compile
-- this library code *and* the less time it takes to compile client code. We
-- thus probably want to comment out all or some of the `SPEC`s in `SPECS` while
-- developing.

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

{--------------------------------------------------------------------
    pointed and keys packages
--------------------------------------------------------------------}

instance Applicative (Vec n) => Pointed (Vec n) where
  point = pure

instance (Functor (Vec n), Applicative (Vec n)) => Zip (Vec n) where
  zipWith = liftA2

-- Needs UndecidableInstances

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

-- Needs UndecidableInstances
