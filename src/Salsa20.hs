{-# OPTIONS -fno-warn-name-shadowing #-}
-- |
-- Module      : Salsa20
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- **Don't even dare use this in production holy omg no. Don't even**
-- **use the original TweetNaCl C code. Use the real NaCl. This is**
-- **an art project.**
--
-- Salsa20 is the core primitive base of much of NaCl. This is a pure
-- Haskell implementation based on the doubleround core at
-- <<http://cr.yp.to/salsa20.html>>. It seems accurate against
-- `doubleround/1` examples in the Snuffle Spec
-- <<http://cr.yp.to/snuffle/spec.pdf>> but doesn't match the examples
-- in <<http://cr.yp.to/snuffle/salsafamily-20071225.pdf>>.

module Salsa20 (
  -- * Core interface types
  Key (..), Nonce (..), Block (..),
  freshKey, freshNonce, nextNonce, nextBlock, 
  SalsaStateV (..), SalsaState (..),

  -- * Salsa family
  salsa20, salsa, coreDR, step, quarter,

  -- * Pure specification
  salsaPure20, salsaPure,
  quarterPure, rowPure, colPure, doublePure,
  corePure,
  
  -- * Utilities
  AsWord64 (..)
  ) where

import Data.Bits
import Data.Word

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.Reader

import Data.Monoid
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as SL
import qualified Data.Vector.Unboxed.Mutable   as M
import qualified Data.Vector.Unboxed           as U
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B

import System.IO

class AsWord64 a where
  _Word64 :: Iso' a Word64

-- | [*private*] The Salsa20 key
data Key   = Key   {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32

                   {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32

freshKey :: IO Key
freshKey = do
  h <- openFile "/dev/urandom" ReadMode
  s <- S.hGet h 32
  let (k1:k2:k3:k4:k5:k6:k7:k8:_) =
        runGet (replicateM 8 getWord32le) . SL.fromStrict $ s
  hClose h
  return (Key k1 k2 k3 k4 k5 k6 k7 k8)

instance Show Key where
  show (Key k0 k1 k2 k3 k4 k5 k6 k7) =
    TL.unpack $ B.toLazyText $ mconcat
    [ B.fromString "Key\n[", n
    , i,    h  k0, s,    h  k1, s,    h  k2, s,    h  k3, n
    , i,    h  k4, s,    h  k5, s,    h  k6, s,    h  k7, n
    , B.fromString "]"
    ]
    where h it = B.fromString "0x" <> B.hexadecimal it
          s    = B.fromString "   "
          n    = B.singleton '\n'
          i    = B.fromString "    "

-- | [*public*] The Salsa20 nonce
data Nonce = Nonce {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32

instance AsWord64 Nonce where
  _Word64 = iso to fro where
    to (Nonce n1 n2) = (fromIntegral n1) + shiftL (fromIntegral n2) 32
    {-# INLINE to #-}
    fro w = Nonce (fromIntegral (        w .&.             4294967295 )    )
                  (fromIntegral (shiftR (w .&. (complement 4294967295)) 32))
    {-# INLINE fro #-}
  {-# INLINE _Word64 #-}

freshNonce :: IO Nonce
freshNonce = do
  h <- openFile "/dev/urandom" ReadMode
  s <- S.hGet h 8
  let (n1:n2:_) =
        runGet (replicateM 2 getWord32le) . SL.fromStrict $ s
  hClose h
  return (Nonce n1 n2)

nextNonce :: Nonce -> Nonce
nextNonce (Nonce n1 n2) = Nonce (1 + n1) n2

instance Num Nonce where
  fromInteger n = fromInteger n ^. from _Word64
  n1 + n2  = ((n1 ^. _Word64) + (n2 ^. _Word64)) ^. from _Word64
  n1 * n2  = ((n1 ^. _Word64) * (n2 ^. _Word64)) ^. from _Word64
  abs n    = n
  signum _ = 1

instance Show Nonce where
  show (Nonce n0 n1) =
    TL.unpack $ B.toLazyText $ mconcat
    [ B.fromString "Nonce [   ", h n0, s, h n1, B.fromString "   ]" ]
    where h it = B.fromString "0x" <> B.hexadecimal it
          s    = B.fromString "   "

-- | [*public*] The Salsa20 block location
data Block = Block {-# UNPACK #-} !Word32
                   {-# UNPACK #-} !Word32

nextBlock :: Block -> Block
nextBlock (Block n1 n2) = Block (1 + n1) n2

instance AsWord64 Block where
  _Word64 = iso to fro where
    to (Block b1 b2) = (fromIntegral b1) + shiftL (fromIntegral b2) 32
    {-# INLINE to #-}
    fro w = Block (fromIntegral (        w .&.             4294967295 )    )
                  (fromIntegral (shiftR (w .&. (complement 4294967295)) 32))
    {-# INLINE fro #-}
  {-# INLINE _Word64 #-}

instance Num Block where
  fromInteger b = fromInteger b ^. from _Word64
  b1 + b2  = ((b1 ^. _Word64) + (b2 ^. _Word64)) ^. from _Word64
  b1 * b2  = ((b1 ^. _Word64) * (b2 ^. _Word64)) ^. from _Word64
  abs b    = b
  signum _ = 1

instance Show Block where
  show (Block b0 b1) =
    TL.unpack $ B.toLazyText $ mconcat
    [ B.fromString "Block [   ", h b0, s, h b1, B.fromString "   ]" ]
    where h it = B.fromString "0x" <> B.hexadecimal it
          s    = B.fromString "   "

newtype SalsaStateV = SalsaStateV (U.Vector Word32) deriving (Eq, Ord)

instance Show SalsaStateV where
  show (SalsaStateV v) =
    TL.unpack $ B.toLazyText $ mconcat
    [ B.fromString "SalsaStateV\n[", n
    , i,    h  0, s,    h  1, s,    h  2, s,    h  3, n
    , i,    h  4, s,    h  5, s,    h  6, s,    h  7, n
    , i,    h  8, s,    h  9, s,    h 10, s,    h 11, n
    , i,    h 12, s,    h 13, s,    h 14, s,    h 15, n
    , B.fromString "]"
    ]
    where ws  = U.toList v
          h n = B.fromString "0x" <> B.hexadecimal (ws !! n)
          s   = B.fromString "   "
          n   = B.singleton '\n'
          i   = B.fromString "    "

instance Binary SalsaStateV where
  put (SalsaStateV v) = sequence_ . map putWord32le . U.toList $ v
  get = do vs <- replicateM 16 getWord32le
           return $ SalsaStateV (U.fromList vs)

data SalsaState =
  SalsaState {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32

             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32

             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32

             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
             {-# UNPACK #-} !Word32
  deriving (Eq, Ord)

instance Binary SalsaState where
  put (SalsaState a0  a1  a2  a3
                  a4  a5  a6  a7
                  a8  a9  a10 a11
                  a12 a13 a14 a15 ) =
    sequence_ [ putWord32le a0
              , putWord32le a1
              , putWord32le a2
              , putWord32le a3
                
              , putWord32le a4
              , putWord32le a5
              , putWord32le a6
              , putWord32le a7
                
              , putWord32le a8
              , putWord32le a9
              , putWord32le a10
              , putWord32le a11
                
              , putWord32le a12
              , putWord32le a13
              , putWord32le a14
              , putWord32le a15
              ]
    
  get = do (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:_) <- replicateM 16 getWord32le
           return $ SalsaState a b c d
                               e f g h
                               i j k l
                               m n o p

instance Num SalsaState where
  (+) (SalsaState a0  a1  a2  a3
                  a4  a5  a6  a7
                  a8  a9  a10 a11
                  a12 a13 a14 a15 )
      (SalsaState b0  b1  b2  b3
                  b4  b5  b6  b7
                  b8  b9  b10 b11
                  b12 b13 b14 b15 ) =
    SalsaState (a0 + b0)   (a1 + b1)   (a2 + b2)   (a3 + b3)
               (a4 + b4)   (a5 + b5)   (a6 + b6)   (a7 + b7)
               (a8 + b8)   (a9 + b9)   (a10 + b10) (a11 + b11)
               (a12 + b12) (a13 + b13) (a14 + b14) (a15 + b15)

  (*) (SalsaState a0  a1  a2  a3
                  a4  a5  a6  a7
                  a8  a9  a10 a11
                  a12 a13 a14 a15 )
      (SalsaState b0  b1  b2  b3
                  b4  b5  b6  b7
                  b8  b9  b10 b11
                  b12 b13 b14 b15 ) =
    SalsaState (a0 * b0)   (a1 * b1)   (a2 * b2)   (a3 * b3)
               (a4 * b4)   (a5 * b5)   (a6 * b6)   (a7 * b7)
               (a8 * b8)   (a9 * b9)   (a10 * b10) (a11 * b11)
               (a12 * b12) (a13 * b13) (a14 * b14) (a15 * b15)

  abs = id
  signum _ = SalsaState 1 1 1 1
                        1 1 1 1
                        1 1 1 1
                        1 1 1 1

  fromInteger n = SalsaState (fromInteger n) 0 0 0
                             0               0 0 0
                             0               0 0 0
                             0               0 0 0

instance Show SalsaState where
  show (SalsaState s0  s1  s2  s3
                   s4  s5  s6  s7
                   s8  s9  s10 s11
                   s12 s13 s14 s15 ) =
    TL.unpack $ B.toLazyText $ mconcat
    [ B.fromString "SalsaState\n[", n
    , i,   h s0  , s,   h s1  , s,   h s2  , s,   h s3  , n
    , i,   h s4  , s,   h s5  , s,   h s6  , s,   h s7  , n
    , i,   h s8  , s,   h s9  , s,   h s10 , s,   h s11 , n
    , i,   h s12 , s,   h s13 , s,   h s14 , s,   h s15 , n
    , B.fromString "]"
    ]
    where h n = B.fromString "0x" <> B.hexadecimal n
          s   = B.fromString "   "
          n   = B.singleton '\n'
          i   = B.fromString "    "

-- k = Key   0x04030201 0x08070605 0x0c0b0a09 0x100f0e0d
--           0x14131211 0x18171615 0x1c1b1a19 0x201f1e1d
-- n = Nonce 0x100f0e0d 0x3320646e
-- b = Block 0x00000007 0x00000000


-- Real Salsa begins here
-------------------------

salsa20 :: PrimMonad m => Key -> Nonce -> Block -> m SalsaStateV
salsa20 = salsa 10 

salsa :: PrimMonad m => Int -> Key -> Nonce -> Block -> m SalsaStateV
salsa m (Key k1 k2 k3 k4 k5 k6 k7 k8) (Nonce n1 n2) (Block b1 b2) = do
  st <- U.thaw state0
  coreDR m st
  SalsaStateV `liftM` U.freeze st
  where
    state0 = U.fromListN 16
      [ 0x61707865 , k1         , k2         , k3
      , k4         , 0x3320646e , n1         , n2
      , b1         , b2         , 0x79622d32 , k5
      , k6         , k7         , k8         , 0x6b206574
      ]

salsaPure20 :: Key -> Nonce -> Block -> SalsaState
salsaPure20 = salsaPure 10

salsaPure :: Int -> Key -> Nonce -> Block -> SalsaState
salsaPure n (Key k0 k1 k2 k3 k4 k5 k6 k7) (Nonce n0 n1) (Block b0 b1) =
  corePure n (SalsaState 0x61707865 k0         k1         k2
                         k3         0x3320646e n0         n1
                         b0         b1         0x79622d32 k4
                         k5         k6         k7         0x6b206574)

-- | 'step' is a primitive Salsa step @x[ i] ^= R(x[ a]+x[ b], r)@.
step :: PrimMonad m =>
        Int -> (Int, Int) -> Int ->
        ReaderT (M.MVector (PrimState m) Word32) m ()
step i (a, b) r = do
  x <- ask
  lift $ do xa <- M.unsafeRead x a
            xb <- M.unsafeRead x b
            xi <- M.unsafeRead x i
            M.unsafeWrite x i $ xor xi $ rotateL (xa + xb) r

-- | Pure specification of the quarter-round function
-- 
-- prop> quarterPure (0x00000000, 0x00000000, 0x00000000, 0x00000000)
--         == (0x00000000, 0x00000000, 0x00000000, 0x00000000)
-- prop> quarterPure (0x00000001, 0x00000000, 0x00000000, 0x00000000)
--         == (0x08008145,0x00000080,0x00010200,0x20500000)
-- prop> quarterPure (0x00000000, 0x00000001, 0x00000000, 0x00000000)
--         == (0x88000100, 0x00000001, 0x00000200, 0x00402000)
-- prop> quarterPure (0x00000000, 0x00000000, 0x00000001, 0x00000000)
--         == (0x80040000, 0x00000000, 0x00000001, 0x00002000)
-- prop> quarterPure (0x00000000, 0x00000000, 0x00000000, 0x00000001)
--         == (0x00048044, 0x00000080, 0x00010000, 0x20100001)
quarterPure :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
quarterPure (y0, y1, y2, y3) =
  let z1 = y1 `xor` ((y0 + y3) `rotateL` 7 )
      z2 = y2 `xor` ((z1 + y0) `rotateL` 9 )
      z3 = y3 `xor` ((z2 + z1) `rotateL` 13)
      z0 = y0 `xor` ((z3 + z2) `rotateL` 18)
  in (z0, z1, z2, z3)

-- | Pure specification of the row-round function
-- 
-- prop> rowPure ( SalsaState 0x00000001 0x00000000 0x00000000 0x00000000
--                            0x00000001 0x00000000 0x00000000 0x00000000
--                            0x00000001 0x00000000 0x00000000 0x00000000
--                            0x00000001 0x00000000 0x00000000 0x00000000
--               ) ==
--               ( SalsaState 0x08008145 0x00000080 0x00010200 0x20500000
--                            0x20100001 0x00048044 0x00000080 0x00010000
--                            0x00000001 0x00002000 0x80040000 0x00000000
--                            0x00000001 0x00000200 0x00402000 0x88000100
--               )
rowPure :: SalsaState -> SalsaState
rowPure (SalsaState y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) =
  let (z0, z1, z2, z3)     = quarterPure (y0, y1, y2, y3)
      (z5, z6, z7, z4)     = quarterPure (y5, y6, y7, y4)
      (z10, z11, z8, z9)   = quarterPure (y10, y11, y8, y9)
      (z15, z12, z13, z14) = quarterPure (y15, y12, y13, y14)
  in SalsaState  z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15

-- | Pure specification of the row-round function
-- 
-- prop> colPure ( SalsaState 0x00000001 0x00000000 0x00000000 0x00000000
--                            0x00000001 0x00000000 0x00000000 0x00000000
--                            0x00000001 0x00000000 0x00000000 0x00000000
--                            0x00000001 0x00000000 0x00000000 0x00000000
--               ) ==
--               ( SalsaState 0x10090288 0x00000000 0x00000000 0x00000000
--                            0x00000101 0x00000000 0x00000000 0x00000000
--                            0x00020401 0x00000000 0x00000000 0x00000000
--                            0x40a04001 0x00000000 0x00000000 0x00000000
colPure :: SalsaState -> SalsaState
colPure (SalsaState x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) =
  let (y0,  y4,  y8,  y12) = quarterPure (x0,  x4,  x8,  x12)
      (y5,  y9,  y13, y1 ) = quarterPure (x5,  x9,  x13, x1 )
      (y10, y14, y2,  y6 ) = quarterPure (x10, x14, x2,  x6 )
      (y15, y3,  y7,  y11) = quarterPure (x15, x3,  x7,  x11)
  in SalsaState y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15

doublePure :: SalsaState -> SalsaState
doublePure = rowPure . colPure

-- | @corePure n v@ performs the @Salsa(2n)@ core mixing process on
-- the 16-word Salsa state @v@ via @n@ doublerounds.
corePure :: Int -> SalsaState -> SalsaState
corePure n s = s + appEndo (mconcat (replicate n (Endo doublePure))) s

-- | Performs a quarter-round operation over 4 indicies on a mutable
-- 'SalsaStateV' vector. The bounds are unchecked.
quarter :: PrimMonad m => (Int, Int, Int, Int) -> M.MVector (PrimState m) Word32 -> m ()
quarter (a, b, c, d) x = flip runReaderT x $ do
  step b (a, d) 7
  step c (b, a) 9
  step d (c, b) 13
  step a (d, c) 18

-- | @core n v@ performs the @Salsa(2n)@ core mixing process on the
-- 16-word Salsa state @v@ via @n@ doublerounds.
coreDR :: PrimMonad m => Int -> M.MVector (PrimState m) Word32 -> m ()
coreDR n x = do
  -- Maintain the old state for the final step
  previous <- M.new 16 :: PrimMonad m => m (M.MVector (PrimState m) Word32)
  M.copy previous x

  -- Perform the core diffusion stages
  replicateM_ n $ flip runReaderT x $ do
      step  4 ( 0, 12) 7      ;    step  8 ( 4,  0) 9
      step 12 ( 8,  4) 13     ;    step  0 (12,  8) 18
      step  9 ( 5,  1) 7      ;    step 13 ( 9,  5) 9
      step  1 (13,  9) 13     ;    step  5 ( 1, 13) 18
      step 14 (10,  6) 7      ;    step  2 (14, 10) 9
      step  6 ( 2, 14) 13     ;    step 10 ( 6,  2) 18
      step  3 (15, 11) 7      ;    step  7 ( 3, 15) 9
      step 11 ( 7,  3) 13     ;    step 15 (11,  7) 18
      step  1 ( 0,  3) 7      ;    step  2 ( 1,  0) 9
      step  3 ( 2,  1) 13     ;    step  0 ( 3,  2) 18
      step  6 ( 5,  4) 7      ;    step  7 ( 6,  5) 9
      step  4 ( 7,  6) 13     ;    step  5 ( 4,  7) 18
      step 11 (10,  9) 7      ;    step  8 (11, 10) 9
      step  9 ( 8, 11) 13     ;    step 10 ( 9,  8) 18
      step 12 (15, 14) 7      ;    step 13 (12, 15) 9
      step 14 (13, 12) 13     ;    step 15 (14, 13) 18

  -- Add the previous state
  forM_ [0..15] $ \i -> do
    previ <- M.unsafeRead previous i
    xi    <- M.unsafeRead x        i
    M.unsafeWrite x i (xi + previ)
