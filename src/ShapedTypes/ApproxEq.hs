{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.ApproxEq
-- Copyright   :  (c) 2015-2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- 
----------------------------------------------------------------------

module ShapedTypes.ApproxEq (ApproxEq(..), approxEqFoldable) where

-- TODO: explicit exports

import Data.Foldable (toList)
import Circat.Complex

-- import Data.Newtypes.PrettyDouble (PrettyDouble(..))

infix 4 =~
class ApproxEq a where
  (=~) :: a -> a -> Bool

closeNum :: (Ord a, Fractional a) => a -> a -> Bool
closeNum x y = abs (x - y) < 1.0e-3

instance ApproxEq Float  where (=~) = closeNum
instance ApproxEq Double where (=~) = closeNum

instance ApproxEq a => ApproxEq (Complex a) where
  (a :+ b) =~ (a' :+ b') = a =~ a' && b =~ b'

-- -- PrettyDouble Eq already works this way
-- instance ApproxEq PrettyDouble where (=~) = (==)

instance ApproxEq a => ApproxEq [a] where
  as =~ bs = length as == length bs && and (zipWith (=~) as bs)

approxEqFoldable :: (ApproxEq a, Foldable f) => f a -> f a -> Bool
approxEqFoldable as bs = toList as =~ toList bs
