{-# LANGUAGE CPP, TypeOperators #-}
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

-- | Add post- and pre-processing
(<--) :: (b -> b') -> (a' -> a) -> ((a -> b) -> (a' -> b'))
(h <-- f) g = h . g . f
{-# INLINE (<--) #-}

-- | Operate inside a Generic1
inGeneric1 :: (Generic1 f, Generic1 g) => (Rep1 f a -> Rep1 g b) -> (f a -> g b)
inGeneric1 = to1 <-- from1

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

-- | Apply a unary function within the 'O' constructor.
inComp :: (g (f a) -> g' (f' a')) -> ((g :.: f) a -> (g' :.: f') a')
inComp = Comp1 <-- unComp1

-- | Apply a binary function within the 'Comp1' constructor.
inComp2 :: (  g (f a)   -> g' (f' a')     -> g'' (f'' a''))
        -> ((g :.: f) a -> (g' :.: f') a' -> (g'' :.: f'') a'')
inComp2 = inComp <-- unComp1
