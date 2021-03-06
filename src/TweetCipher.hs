{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TweetCipher
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
-- TweetCipher is an art project or even a joke. Do not use it.
-- 
-- <<http://blog.cryptographyengineering.com/2013/07/tweetnacl.html>>
-- 
module TweetCipher where

import System.Posix.Env.ByteString

import Control.Lens
import Control.Proxy
import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Lazy.Internal as SI
import qualified Data.ByteString as St
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Binary.Get
import Data.Bits
import Data.Word

-- | Side-effecting loop
loop :: (Monad m) => Int -> (Int -> m b) -> m ()
loop n = forM_ [0..(pred n)]

-- | #define R(v,n)(((v)<<(64-n))|((v)>>n))
rf :: Bits a => a -> Int -> a
rf = rotateR

axrf :: M.IOVector Word64 -> Int -> Int -> Int -> Int -> IO ()
axrf x a b c r = do xb <- M.read x b
                    xa <- M.read x a
                    let new_xa = xa + xb
                    M.write x a new_xa
                    xc <- M.read x c
                    M.write x c (rf (xc `xor` new_xa) r)

gf :: M.IOVector Word64 -> Int -> Int -> Int -> Int -> IO ()
gf x a b c d = sequence_ [ axrf x a b d 32
                         , axrf x c d b 25
                         , axrf x a b d 16
                         , axrf x c d b 11
                         ]

roundsf :: M.IOVector Word64 -> IO ()
roundsf x = replicateM_ 6 $ do
  loop 4 $ \i -> do
    gf x i (i+4) (i+8) (i+12)
  loop 4 $ \j -> do
    gf x j
         ((j+1) `mod` 4 + 4)
         ((j+2) `mod` 4 + 8)
         ((j+3) `mod` 4 + 12)

get64s :: Int -> S.ByteString -> V.Vector Word64
get64s n = V.fromListN n . runGet (replicateM n getWord64host)

writeTo :: M.IOVector Word64 -> Int -> V.Vector Word64 -> IO ()
writeTo m di v = forM_ [0..(V.length v - 1)] $ \i -> do
  a <- V.indexM v i
  M.write m (i + di) a

contents :: Proxy p => r -> Producer p St.ByteString IO r
contents r = runIdentityP $ do
  bs <- lift $ S.getContents
  SI.foldrChunks (\e a -> respond e >> a) (return r) bs

printer :: Proxy p => () -> p () St.ByteString b' b IO r
printer = runIdentityK $ foreverK $ \_ -> request () >>= lift . St.putStr

word8s :: Proxy p => (Word8 -> IO Word8) ->
          () -> Pipe p St.ByteString St.ByteString IO ()
word8s f = mapMD bsMapM where
  bsMapM = fmap St.pack . liftM reverse . St.foldl' (\a w -> (:) <$> f w <*> a) (return [])

type Key   = V.Vector Word64
type Nonce = V.Vector Word64

-- | Initialize the sponge state using a repeated initializer
-- ("twithasi" as a 'Word64'), copy in the key and nonce as the first
-- 48 bytes, then shuffle using 5 'roundsf'.
-- -------------------------------------------------------------------
-- Note that the sponge state is only 1024 bits unlike the SHA3-Keccak
-- itself which uses 1600. What is the capacity? (16*64 - 8 bits) What
-- is the bitrate? (1 8-bit char) What is the padding rule? (Only
-- accept character-sized inputs) What is the permutation function?
-- (ROUNDS)
--
--     In the hermetic sponge strategy, the capacity determines the
--     claimed level of security, and one can trade claimed security
--     for speed by increasing the capacity c and decreasing the
--     bitrate r accordingly, or vice-versa. When a padding rule is
--     used with particular properties, one can securely instantiate
--     sponge functions with different rates with the same
--     fixed-length permutation. The simplest padding rule satisfying
--     these properties is called the multi-rate padding: it appends a
--     single 1-bit, then a variable number of zeroes and finally
--     another 1-bit.
-- 
-- <<http://sponge.noekeon.org/>>
-- -------------------------------------------------------------------
initState :: Key -> Nonce -> IO (M.IOVector Word64)
initState key nonce = do
  x <- V.thaw $ V.generate 16 ((* 0x7477697468617369) . fromIntegral)
  writeTo x 0 key
  writeTo x 4 nonce
  roundsf x
  return x

-- | Lens to peer at the first byte of a Word64.
_Byte1 :: Lens' Word64 Word8
_Byte1 = lens (\w      -> fromIntegral (w .&. 255))
              (\w head -> (w .&. (complement 255)) .|. (fromIntegral head))

-- | duplex
-- -------------------------------------------------------------------
-- <<http://sponge.noekeon.org/SpongeDuplex.pdf>>
-- -------------------------------------------------------------------
duplex :: Proxy p =>
          Bool -> Key -> Nonce ->
          () -> Pipe p St.ByteString St.ByteString IO () 
duplex isEncrypt key nonce () = runIdentityP $ do
  state <- lift $ initState key nonce
  word8s (eachWord state) ()
  where
    eachWord state w = do
      x0 <- M.read state 0
      M.write state 1
        $ if isEncrypt
          then x0 & _Byte1 %~ xor w
          else xor (fromIntegral w) (x0 & _Byte1 .~ 0)
      roundsf state
      return $ x0 ^. _Byte1 . to (xor w)

main :: IO ()
main = do
  (mode:keyS:nonceS:_) <- getArgs
  let key   = get64s 4 $ S.fromStrict keyS
      nonce = get64s 2 $ S.fromStrict nonceS
      isEncrypt = mode == "e"
  runProxy $ contents
             >-> duplex isEncrypt key nonce
             >-> printer
