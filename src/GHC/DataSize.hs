{- |
   Module      : GHC.DataSize
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de
 -}
module GHC.DataSize (
  closureSize,
  recursiveSize
  )
  where

import GHC.HeapView hiding (size)

import GHC.Constants (wORD_SIZE)

import Control.Monad

import System.Mem

-- Inspired by Simon Marlow:
-- https://ghcmutterings.wordpress.com/2009/02/12/53/

-- | Calculate size of GHC objects in Bytes. Note that an object may not be
--   evaluated yet and only the size of the initial closure is returned.
closureSize :: Num b => a -> IO b
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
recursiveSize :: Num b => a -> IO b
recursiveSize x = do
  performGC
  liftM snd $ go ([], 0) $ asBox x
  where go (vs, acc) b@(Box y)
          | b `elem` vs = return (vs, acc)
          | otherwise   = do
             size    <- closureSize y
             closure <- getClosureData y
             --putStrLn $ (show b) ++ " | " ++ (show closure)
             foldM go (b : vs, acc + size) $ allPtrs closure
