{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Sponge
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Sponge constructions built on Folds.

module Sponge where

import L
import Salsa20
import Data.Monoid
import Data.Word
import Data.Bits
import Data.Stream.Infinite as I
import Control.Lens

{- |

A sponge construction consumes some number of a's then emits some
number of b's.

-}

class    Permutes s     where permute :: s -> s
instance Permutes Salsa where permute = salsa20

sponge :: (Permutes s, Bits a) => Lens' s a -> s -> L a (Stream a)
sponge l s = Fold (\s a -> permute $ over l (xor a) s) s (I.unfold $ \s -> (view l s, permute s))
