{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Orphans
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Orphan instances
----------------------------------------------------------------------

module ShapedTypes.Orphans () where

import Prelude hiding (zip,zipWith)

import Control.Applicative (liftA2)
import GHC.Generics

import Data.Key

import ShapedTypes.Misc (inComp,inComp2)

{--------------------------------------------------------------------
    Par1
--------------------------------------------------------------------}

instance Zip Par1 where zipWith = liftA2

type instance Key Par1 = ()

instance Keyed Par1 where mapWithKey q = fmap (q ())

instance ZipWithKey Par1

instance Lookup Par1 where lookup = lookupDefault

instance Indexable Par1 where index (Par1 a) () = a

instance Adjustable Par1 where adjust h () = fmap h


{--------------------------------------------------------------------
    Comp1
--------------------------------------------------------------------}

instance (Zip f, Zip g) => Zip (g :.: f) where
  zipWith = inComp2 . zipWith . zipWith

#if 0
h :: a -> b -> c
zipWith h :: f a -> f b -> f c
zipWith (zipWith h) :: g (f a) -> g (f b) -> g (f c)
#endif

type instance Key (g :.: f) = (Key g , Key f)

instance (Keyed g, Keyed f) => Keyed (g :.: f) where
  mapWithKey q = inComp (mapWithKey (mapWithKey . fmap q . (,)))

#if 0
mapWithKey :: (Key (g :.: f) -> a -> b) -> (g :.: f) a -> (g :.: f) b
           :: ((Key g, Key f) -> a -> b) -> (g :.: f) a -> (g :.: f) b

q   :: ((Key g, Key f) -> a -> b)
gfa :: g (f a)
gk  :: Key g
fk  :: Key f

mapWithKey q
  = \ (Comp1 gfa) -> Comp1 (mapWithKey (\ gk -> mapWithKey (\ fk a -> q (gk, fk) a)) gfa)
  = inComp $ mapWithKey (\ gk -> mapWithKey (\ fk a -> q (gk, fk) a))
  = inComp $ mapWithKey (\ gk -> mapWithKey (\ fk -> q (gk, fk)))
  = inComp $ mapWithKey (\ gk -> mapWithKey (q . (gk,)))
  = inComp $ mapWithKey (\ gk -> mapWithKey . (q .) $ (gk,))
  = inComp $ mapWithKey (\ gk -> mapWithKey . (q .) $ (,) gk)
  = inComp (mapWithKey (mapWithKey . fmap q . (,)))
#endif

instance (Keyed g, Zip g, Keyed f, Zip f) => ZipWithKey (g :.: f)

instance (Indexable g, Indexable f) => Lookup (g :.: f) where
  lookup = lookupDefault

instance (Indexable g, Indexable f) =>
         Indexable (g :.: f) where
  index (Comp1 gfa) (gk,fk) = gfa ! gk ! fk

instance (Adjustable g, Adjustable f) => Adjustable (g :.: f) where
  adjust h (gk,fk) = inComp (adjust (adjust h fk) gk)
