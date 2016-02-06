-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  ShapedTypes.Interactive
-- Copyright   :  (c) 2016 Conal Elliott
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Non-interactive HERMIT-based plugin
----------------------------------------------------------------------

-- TODO: Move back to lambda-ccc when done

module ShapedTypes.Plugin where

-- TODO: explicit exports

import Prelude hiding ((.))

import Control.Category ((.))
import GhcPlugins (Plugin)
import Language.KURE (tryR)
import HERMIT.Kernel (CommitMsg(..))
import HERMIT.Plugin

import Monomorph.Plugin (tweakPretty)
import LambdaCCC.Reify (reifyModule)

plugin :: Plugin
plugin = hermitPlugin (lastPass . const plug)  -- pass 0
 where
   plug = tweakPretty >> apply (Always "reify") (tryR reifyModule)
