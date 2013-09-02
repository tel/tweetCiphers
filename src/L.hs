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

data L a b = forall x. L (x -> a -> x) (x -> b) x

data P a b = P !a !b

instance Functor (L a) where fmap f (L xax xb x) = L xax (f . xb) x
instance Applicative (L a) where
  pure a = L const (const a) ()
  L xax xf x <*> L yay yb y =
    L (\(P x y) a -> P (xax x a) (yay y a))
      (\(P x y) -> xf x (yb y))
      (P x y)

instance Comonad (L a) where
  extract (L _ xb x) = xb x
  duplicate (L xax xb x) = L xax (\x' -> L xax xb x') x

instance Profunctor L where
  dimap f g (L xax xb x) = L (\x -> xax x . f) (g . xb) x

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
