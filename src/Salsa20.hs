
module Salsa20 where

import Data.Bits
import Data.Word

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Trans

import Data.Monoid
import Data.Binary.Get
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
    fro w = Nonce (fromIntegral (        w .&.             (2^32 - 1) )    )
                  (fromIntegral (shiftR (w .&. (complement (2^32 - 1))) 32))
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
    fro w = Block (fromIntegral (        w .&.             (2^32 - 1) )    )
                  (fromIntegral (shiftR (w .&. (complement (2^32 - 1))) 32))
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

newtype SalsaState = SalsaState (U.Vector Word32)

instance Show SalsaState where
  show (SalsaState v) =
    TL.unpack $ B.toLazyText $ mconcat
    [ B.fromString "SalsaState\n[", n
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

k = Key   0x04030201 0x08070605 0x0c0b0a09 0x100f0e0d
          0x14131211 0x18171615 0x1c1b1a19 0x201f1e1d
n = Nonce 0x100f0e0d 0x3320646e
b = Block 0x00000007 0x00000000

salsa :: PrimMonad m => Int -> Key -> Nonce -> Block -> m SalsaState
salsa m (Key k1 k2 k3 k4 k5 k6 k7 k8) (Nonce n1 n2) (Block b1 b2) = do
  st <- U.thaw state0
  core m st
  SalsaState `liftM` U.freeze st
  where
    state0 = U.fromListN 16
      [ 0x61707865 , k1         , k2         , k3
      , k4         , 0x3320646e , n1         , n2
      , b1         , b2         , 0x79622d32 , k5
      , k6         , k7         , k8         , 0x6b206574
      ]

-- | @core n v@ performs the @Salsa(2n)@ core mixing process on the
-- 16-word Salsa state @v@.
core :: PrimMonad m => Int -> M.MVector (PrimState m) Word32 -> m ()
core n x = do
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
  where
    -- x[ i] ^= R(x[ a]+x[ b], r)
    step :: PrimMonad m =>
            Int -> (Int, Int) -> Int ->
            ReaderT (M.MVector (PrimState m) Word32) m ()
    step i (a, b) r = do
      x <- ask
      lift $ do xa <- M.unsafeRead x a
                xb <- M.unsafeRead x b
                xi <- M.unsafeRead x i
                M.unsafeWrite x i $ xor xi $ rotateL (xa + xb) r
