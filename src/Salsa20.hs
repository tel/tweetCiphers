{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Salsa20
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Implementation of Salsa20.

module Main where

import Control.Applicative

import GHC.Generics

import Data.Functor.Identity
import Data.AdditiveGroup

import Data.Word
import Data.Bits
import Data.Bytes.Serial

main :: IO ()
main = salsa20 (Salsa 1 2 3 4 5 6 7 8 9 10 12 13 14 15 16)

salsa20 :: Salsa -> Salsa
salsa20 s = (s ^+^)
          . double . double . double . double . double -- 10 double-rounds
          . double . double . double . double . double
          $ s

data Salsa =
  Salsa {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32

        {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Word32   {-# UNPACK #-} !Word32
  deriving (Generic, Show, Eq, Ord)

instance Serial Salsa

instance AdditiveGroup Salsa where
  zeroV = Salsa 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  -- Writing this out so it's easier for the compiler to inline
  (^+^) (Salsa a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af)
        (Salsa b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf) =
    (Salsa (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)
           (a4 + b4) (a5 + b5) (a6 + b6) (a7 + b7)
           (a8 + b8) (a9 + b9) (aa + ba) (ab + bb)
           (ac + bc) (ad + bd) (ae + be) (af + bf) )
  negateV = runIdentity . salsaBytes (Identity . negate)

salsaBytes :: Applicative f => (Word32 -> f Word32) -> Salsa -> f Salsa
salsaBytes inj (Salsa a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af) =
  Salsa <$> inj a0 <*> inj a1 <*> inj a2 <*> inj a3
        <*> inj a4 <*> inj a5 <*> inj a6 <*> inj a7
        <*> inj a8 <*> inj a9 <*> inj aa <*> inj ab
        <*> inj ac <*> inj ad <*> inj ae <*> inj af

data Quarter =
  Quarter {-# UNPACK #-} !Word32
          {-# UNPACK #-} !Word32
          {-# UNPACK #-} !Word32
          {-# UNPACK #-} !Word32
  deriving (Generic, Show, Eq, Ord)

instance Serial Quarter

quarter :: Quarter -> Quarter
quarter (Quarter y0 y1 y2 y3) =
  let z1 = xor y1 $ rotateL (y0 + y3) 7
      z2 = xor y2 $ rotateL (z1 + y0) 9
      z3 = xor y3 $ rotateL (z2 + z1) 13
      z0 = xor y0 $ rotateL (z3 + z2) 18
  in Quarter z0 z1 z2 z3

row :: Salsa -> Salsa
row (Salsa y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 ya yb yc yd ye yf ) =
  let Quarter z0 z1 z2 z3 = quarter $ Quarter y0 y1 y2 y3
      Quarter z5 z6 z7 z4 = quarter $ Quarter y5 y6 y7 y4
      Quarter za zb z8 z9 = quarter $ Quarter ya yb y8 y9
      Quarter zf zc zd ze = quarter $ Quarter yf yc yd ye
  in Salsa z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 za zb zc zd ze zf

column :: Salsa -> Salsa
column (Salsa x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xa xb xc xd xe xf) =
  let Quarter y0 y4 y8 yc = quarter $ Quarter x0 x4 x8 xc
      Quarter y5 y9 yd y1 = quarter $ Quarter x5 x9 xd x1
      Quarter ya ye y2 y6 = quarter $ Quarter xa xe x2 x6
      Quarter yf y3 y7 yb = quarter $ Quarter xf x3 x7 xb
  in Salsa y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 ya yb yc yd ye yf

double :: Salsa -> Salsa
double = row . column
