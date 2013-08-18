
-- |
-- Module      : TweetNaCl
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
-- TweetNaCl is an art project demonstrating that one need never lift
-- archaic crypto code off Wikipedia. At best you should just use a
-- wonderful, high-level crypto library like NaCl. At worst, TweetNaCl
-- is no longer than what you'll find elsewhere so use it instead.
-- 
-- <<http://blog.cryptographyengineering.com/2013/07/tweetnacl.html>>
-- 
-- Unfortunately for non-usable crypto code, it's still written in
-- C. In order to explore the code in more detail and see the
-- algorithm laid out at a higher level this is the same algorithm
-- translated to beautiful Haskell.

module NaCl where

import Data.Bits
import Data.Word
