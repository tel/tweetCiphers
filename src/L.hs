{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : L
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Minimalist reimplementation of Salsa20.

module L where

import Control.Applicative
import Control.Comonad

import Data.Profunctor

data L a b = forall x. L (x -> a -> x) x (x -> b)

data P a b = P !a !b

instance Functor (L a) where fmap f (L xax x xb) = L xax x (f . xb)
instance Applicative (L a) where
  pure = L const () . const
  L xax x xf <*> L yay y yb =
    L (\(P x y) a -> P (xax x a) (yay y a))
      (P x y)
      (\(P x y) -> xf x (yb y))

instance Comonad (L a) where
  extract (L _ x xb) = xb x
  duplicate (L xax x xb) = L xax x $ \x' -> L xax x' xb

instance Profunctor L where
  dimap f g (L xax x xb) = L (\x -> xax x . f) x (g . xb)

instance Num b => Num (L a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (L a b) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational
