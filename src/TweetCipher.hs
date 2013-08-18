
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
-- Unfortunately for non-usable crypto code, it's still written in
-- C. In order to explore the code in more detail and see the
-- algorithm laid out at a higher level this is the same algorithm
-- translated to beautiful Haskell.

module TweetCipher where

import System.Posix.Env.ByteString

import Control.Proxy
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive

import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Lazy.Internal as SI
import qualified Data.ByteString as St
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Vector.Generic.New
import Data.Binary.Get
import Data.Bits
import Data.Word

-- #define LOOP(n) for(i=0;i<n;++i)

-- | Side-effecting loop
loop :: (Monad m) => Int -> (Int -> m b) -> m ()
loop n = forM_ [0..(pred n)]

-- #define W(v,n) ((uint64_t*)v)[n]
--
-- interpret v as a [Word64] and look for the nth
-- entry. 'lookupNth :: a -> n -> Word64'

-- | #define R(v,n)(((v)<<(64-n))|((v)>>n))
rf :: Bits a => a -> Int -> a
rf v n = unsafeShiftL v (64 - n)  .|. unsafeShiftR v n

-- #define AXR(a,b,c,r) x[a] += x[b] ; x[c] = R(x[c]^x[a],r);
axrf :: M.IOVector Word64 -> Int -> Int -> Int -> Int -> IO ()
axrf x a b c r = do xb <- M.unsafeRead x b
                    xa <- M.unsafeRead x a
                    let new_xa = xa + xb
                    M.unsafeWrite x a new_xa
                    xc <- M.unsafeRead x c
                    M.unsafeWrite x c (rf (xc `xor` new_xa) r)

-- #define G(a,b,c,d) {AXR(a,b,d,32) AXR(c,d,b,25) AXR(a,b,d,16) AXR(c,d,b,11)} 
gf :: M.IOVector Word64 -> Int -> Int -> Int -> Int -> IO ()
gf x a b c d = sequence_ [ axrf x a b d 32
                         , axrf x c d b 25
                         , axrf x a b d 16
                         , axrf x c d b 11
                         ]

-- This loop is quite annoying, @(r--)@ decrements @r@ and returns its
-- previous value. If that value is zero then the loop
-- terminates. This means that the loop executes 6 times and observes
-- @r@ in @[5,4,3,2,1,0]@.
-- 
-- #define ROUNDS {for(r=6;r--;){LOOP(4) G(i,i+4,i+8,i+12) \
--                               LOOP(4) G(i,(i+1)%4+4,(i+2)%4+8,(i+3)%4+12)}}

roundsf :: M.IOVector Word64 -> IO ()
roundsf x = forM_ [5,4..0] $ \r -> do
  loop 4 $ \i -> do
    gf x i (i+4) (i+8) (i+12)
    loop 4 $ \j -> do
      gf x j
           ((j+1) `mod` 4 + 4)
           ((j+2) `mod` 4 + 8)
           ((j+3) `mod` 4 + 12)

get64s :: Int -> S.ByteString -> V.Vector Word64
get64s n = V.fromListN n . runGet (replicateM n getWord64host)

writeTo
  :: (PrimMonad m, M.Unbox a) =>
     M.MVector (PrimState m) a -> Int -> V.Vector a -> m [()]
writeTo m di v = forM [0..V.length v] $ \i -> do
  a <- V.unsafeIndexM v i
  M.unsafeWrite m (i + di) a

contents :: Proxy p => r -> p a' a a1 St.ByteString IO r
contents r = runIdentityP $ do
  bs <- lift $ S.getContents
  SI.foldrChunks (\e a -> respond e >> a) (return r) bs

printer :: Proxy p => () -> p () St.ByteString b' b IO r
printer = runIdentityK $ foreverK $ \_ -> request () >>= lift . St.putStr

word8s
  :: Proxy p =>
     (Word8 -> IO Word8) -> () -> Pipe p St.ByteString St.ByteString IO ()
word8s f = mapMD bsMapM where
  bsMapM = fmap St.pack . St.foldl' (\a w -> (:) <$> f w <*> a) (return [])

main :: IO ()
main = do
  (mode:key:nonce:_) <- getArgs
  let isEncrypt = mode == "e"

  -- * Initialize the sponge state
  -- -----------------------------------------------------------------
  -- Note that the sponge state is only 1024 bits unlike the
  -- SHA3-Keccak itself which uses 1600. What is the capacity? (16*64
  -- - 8 bits) What is the bitrate? (1 8-bit char) What is the padding
  -- rule? (Only accept character-sized inputs) What is the
  -- permutation function? (ROUNDS)
  --
  --     In the hermetic sponge strategy, the capacity determines the
  --     claimed level of security, and one can trade claimed security
  --     for speed by increasing the capacity c and decreasing the
  --     bitrate r accordingly, or vice-versa. When a padding rule is
  --     used with particular properties, one can securely instantiate
  --     sponge functions with different rates with the same
  --     fixed-length permutation. The simplest padding rule
  --     satisfying these properties is called the multi-rate padding:
  --     it appends a single 1-bit, then a variable number of zeroes
  --     and finally another 1-bit.
  --
  -- <<http://sponge.noekeon.org/>>
  -- -----------------------------------------------------------------

  -- 0x7477697468617369 is "twithasi", but it could just be zero?
  x <- V.thaw $ V.generate 16 ((* 0x7477697468617369) . fromIntegral)

  -- Key and nonce reading
  writeTo x 0 (get64s 4 (S.fromStrict key))
  writeTo x 4 (get64s 2 (S.fromStrict nonce))

  -- mix up the state
  roundsf x

  -- duplex
  -- ----------------------------------------------
  -- <<http://sponge.noekeon.org/SpongeDuplex.pdf>>
  -- ----------------------------------------------

  -- if decrypting, terminate on \n... haha!
  --
  --   (! f)  && (10 == ((x[0] ^ c) % 256))

  -- output XOR then, on decrypt, wipe the first 8 bits of x[0].
  --
  -- if isEncrypt then x[0] else x[0] .&. complement (255 :: Word64)

  -- ROUNDS

  --   while( (c=getchar()) != EOF )
  --     if(!f&&10 == (x[0]^c)%256) return 0;
  --     putchar(x[0]^c);                      -- run the XOR twice so as to lose timing leak
  --     x[0]=c^(f?x[0]:x[0]&~255ULL);
  --     ROUNDS;

  -- message termination signal (???)
  --
  --   x[0] ^= 1; 
  --   ROUNDS;

  -- squeeze out an authenticator
  --
  --   LOOP(8) putchar(255 & ( (x[4] ^ x[5]) >> 8*i) ); 
  --   LOOP(8) putchar(255 & ( (x[6] ^ x[7]) >> 8*i) );
