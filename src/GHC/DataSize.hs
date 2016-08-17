{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{- |
   Module      : GHC.DataSize
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de
 -}
module GHC.DataSize (
  closureSize,
  recursiveSize,
  recursiveSizeNF
  )
  where

import Control.DeepSeq (NFData, force)

#if __GLASGOW_HASKELL < 708
import Data.Word (Word)
#endif

import GHC.HeapView hiding (size)

import Control.Monad

import System.Mem

-- This used to be available via GHC.Constants
#include "MachDeps.h"
wORD_SIZE :: Int
wORD_SIZE = SIZEOF_HSWORD

--import qualified Data.IntMap as IntMap

--depth :: Int
--depth = 10^10

-- Inspired by Simon Marlow:
-- https://ghcmutterings.wordpress.com/2009/02/12/53/

-- | Calculate size of GHC objects in Bytes. Note that an object may not be
--   evaluated yet and only the size of the initial closure is returned.
closureSize :: a -> IO Word
closureSize x = do
  (_,y,_) <- getClosureRaw x
  return . fromIntegral $ length y * wORD_SIZE

-- | Calculate the recursive size of GHC objects in Bytes. Note that the actual
--   size in memory is calculated, so shared values are only counted once.
--
--   Call with
--   @
--    recursiveSize $! 2
--   @
--   to force evaluation to WHNF before calculating the size.
--
--   Call with
--   @
--    recursiveSize $!! \"foobar\"
--   @
--   ($!! from Control.DeepSeq) to force full evaluation before calculating the
--   size.
--
--   A garbage collection is performed before the size is calculated, because
--   the garbage collector would make heap walks difficult.
--
--   This function works very quickly on small data structures, but can be slow
--   on large and complex ones. If speed is an issue it's probably possible to
--   get the exact size of a small portion of the data structure and then
--   estimate the total size from that.

recursiveSize :: a -> IO Word
recursiveSize x = do
  performGC
  liftM snd $ go ([], 0) $ asBox x
  where go (!vs, !acc) b@(Box y) = do
          isElem <- liftM or $ mapM (areBoxesEqual b) vs
          if isElem
            then return (vs, acc)
            else do
             size    <- closureSize y
             closure <- getClosureData y
             foldM go (b : vs, acc + size) $ allPtrs closure

-- | Calculate the recursive size of GHC objects in Bytes after calling
-- Control.DeepSeq.force on the data structure to force it into Normal Form.
-- Using this function requires that the data structure has an `NFData`
-- typeclass instance.

recursiveSizeNF :: NFData a => a -> IO Word
recursiveSizeNF = recursiveSize . force
