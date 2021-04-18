{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Main where
  
import Cursor
import Data.Unrestricted.Linear
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Prelude.Linear ((&))
import Examples
import Class
import Types
import GHC.Exts
import GHC.IO
import System.IO.MMap
import Data.Binary

-- (&) :: a %p -> (a %p -> b) %p -> b
-- (&) x f = f x

f :: Int -> Int
f n = sum (map ((2^)) [0..n-1]) + 9*(2^n)

main :: IO ()
main = do
  let n = 29
      l = f n
  bs <- BSL.readFile "tree"
  -- print (bs' == bs)
  -- print (BSL.stripPrefix bs bs')
  -- print (BSL.stripPrefix bs' bs)
  -- print (head $ tail $ BSL.toChunks bs)
  let t = tree n
      -- (bs, _) = writeBufferLazy (2^22) (\wcur -> (Ur (), writeTreeInPlace n wcur))
      -- t' = unsafeReadBuffer bs (\rcur -> readTree rcur & \(Res t rcur) -> consumeCursor rcur & \() -> Ur t)
  -- unsafeMMapWriteBuffer "tree-copy" l (\wc ->
  --   unsafeReadBuffer bs (\rc ->
  --     copyTree rc wc & \(rc,wc) ->
  --       consumeCursor rc & \() ->
  --         consumeCursor wc & \() -> Ur ()))
  -- unsafeMMapWriteBuffer "tree" l (\wcur -> writeTreeInPlace n wcur & \wcur -> consumeCursor wcur & \() -> Ur ())
  -- bs <- mmapFileByteString "tree" Nothing
  -- BS.writeFile "tree" bs
  -- BSL.writeFile "tree-copy" bs
  print (sumBSTreeL bs)
  -- print (sumTreeSlow t')
  -- IO (\s -> case touch# t s of s' -> (# s', () #))
  -- print (BS.length bs)
  -- encodeFile "tree-bin" t
  -- t <- decodeFile "tree-bin"
  -- print (sumTreeSlow t)

