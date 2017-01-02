{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE EmptyCase #-}

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

{-# OPTIONS_GHC -fno-warn-orphans #-}

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Misc
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Miscellany
----------------------------------------------------------------------

-- {-# OPTIONS_GHC -fplugin-opt=ReificationRules.Plugin:trace #-}

-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

module ShapedTypes.Misc where

-- TODO: explicit exports

#if __GLASGOW_HASKELL__ < 800
import Control.Applicative (liftA2)
#endif

import GHC.Generics hiding (C)

import Control.Newtype (Newtype(..))

import Circat.Misc ((:*))

-- | Add post- and pre-processing
(<--) :: (b -> b') -> (a' -> a) -> ((a -> b) -> (a' -> b'))
(h <-- f) g = h . g . f
{-# INLINE (<--) #-}

#if __GLASGOW_HASKELL__ < 800

{--------------------------------------------------------------------
    Orphans
--------------------------------------------------------------------}

-- TODO: Remove when GHC.Generics gets these instances (and more).
-- See <https://ghc.haskell.org/trac/ghc/ticket/9043>.

instance (Functor g, Functor f) => Functor (g :.: f) where
  fmap = inComp . fmap . fmap

instance (Applicative g, Applicative f) => Applicative (g :.: f) where
  pure  = Comp1 . pure . pure
  (<*>) = (inComp2.liftA2) (<*>)

#endif

{--------------------------------------------------------------------
    Misc Generics operations
--------------------------------------------------------------------}

-- | Operate inside a Generic1
inGeneric1 :: (Generic1 f, Generic1 g) => (Rep1 f a -> Rep1 g b) -> (f a -> g b)
inGeneric1 = to1 <-- from1

-- | Apply a unary function within the 'Comp1' constructor.
inComp :: (g (f a) -> g' (f' a')) -> ((g :.: f) a -> (g' :.: f') a')
inComp = Comp1 <-- unComp1

-- | Apply a binary function within the 'Comp1' constructor.
inComp2 :: (  g (f a)   -> g' (f' a')     -> g'' (f'' a''))
        -> ((g :.: f) a -> (g' :.: f') a' -> (g'' :.: f'') a'')
inComp2 = inComp <-- unComp1

absurdF :: V1 a -> b
absurdF = \ case

{--------------------------------------------------------------------
    Newtype
--------------------------------------------------------------------}

-- See <https://github.com/jcristovao/newtype-generics/pull/5>

-- Type generalization of underF from newtype-generics.
underF :: (Newtype n, Newtype n', o' ~ O n', o ~ O n, Functor f, Functor g)
       => (o -> n) -> (f n -> g n') -> (f o -> g o')
underF _ f = fmap unpack . f . fmap pack

-- Type generalization of overF from newtype-generics.
overF :: (Newtype n, Newtype n', o' ~ O n', o ~ O n, Functor f, Functor g)
      => (o -> n) -> (f o -> g o') -> (f n -> g n')
overF _ f = fmap pack . f . fmap unpack

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

(<$*>), mapr :: Functor t => (a -> b) -> (a :* t a) -> (b :* t b)
f <$*> (a,as) = (f a, f <$> as)
mapr = (<$*>)

(<*$>), mapl :: Functor t => (a -> b) -> (t a :* a) -> (t b :* b)
f <*$> (as,a) = (f <$> as, f a)
mapl = (<*$>)
