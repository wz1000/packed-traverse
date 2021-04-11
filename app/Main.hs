{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main where
  
import Cursor
import Data.Unrestricted.Linear
import qualified Data.ByteString as BS
import Prelude.Linear ((&))
import Examples
import Class
import Types

-- (&) :: a %p -> (a %p -> b) %p -> b
-- (&) x f = f x

f :: Int -> Int
f n = sum (map ((*8) . (2^)) [0..n-1]) + 16*(2^n)

main :: IO ()
main = do
  let n = 20
      !l = f n
  let t = tree n
      (!bs,_) = unsafeWriteBuffer l (\wcur -> Res () (serialize t wcur))
      t' = unsafeReadBuffer bs (\rcur -> readTree rcur & \(Res t rcur) -> consumeCursor rcur & \() -> Ur t)
      -- (bs',_) = unsafeWriteBuffer l $ \wc ->
      --   (Ur (), unsafeReadBufferWith bs wc & \(wc,rc) ->
      --             copyTree rc wc & \(rc,wc) ->
      --               consumeCursor rc & \() ->
      --                 wc)
  -- print (BS.length bs')
  -- print (bs' == bs)
  print (sumTreeSlow t')
