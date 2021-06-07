{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
module Main where
  
import Cursor
import Data.Unrestricted.Linear
import Data.ByteString (ByteString)
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
import Data.Binary.Put
import System.Directory

import Criterion.Main

import Control.Monad

-- (&) :: a %p -> (a %p -> b) %p -> b
-- (&) x f = f x

f :: Int -> Int
f n = sum (map ((2^)) [0..n-1]) + 9*(2^n)

fs :: Int -> Int
fs 0 = 1 + 8
fs n = 1 + 8 + 8 + 2*(fs (n-1))

samples :: [Int]
samples = [2,5..26]

main :: IO ()
main = defaultMain
  [ bgroup "write" [ bgroup (show n) [bench name $ nfIO (wf "tree" n) | (name,wf) <- writeBenchs]
                   | n <- samples]
  , bgroup "sum" [env (gen n) $ \dat ->
                    bgroup (show n)
                      [ env (gen' dat) $ \dat -> bench name $ nfIO (verify n $ wf n dat)
                      | (name, wf) <- sumBenchs
                      , let gen'| name /= "binary" = pure
                                | otherwise = \_ -> do
                                   removeFile "tree-bin"
                                   encodeFile "tree-bin" (tree n)
                                   pure ("tree-bin", mempty)
                      ]
                 | n <- samples]
  , bgroup "add1" [env (gen n) $ \ ~(dat,_) ->
                    bgroup (show n)
                      [ env (gen' dat) $ \dat -> bench name $ nfIO (wf n dat)
                      | (name, wf) <- modifyBenchs
                      , let gen'| name /= "binary" = pure
                                | otherwise = \_ -> do
                                   removeFile "tree-bin"
                                   encodeFile "tree-bin" (tree n)
                                   pure "tree-bin"
                      ]
                  | n <- samples]

  , bgroup "skip-write" [bgroup (show n) [bench name $ nfIO (wf ("tree-"++name) n) | (name,wf) <- skipWBenchs ] | n <- samples]
  , bgroup "skip-last" [bgroup (show n) [env (genS name n) $ \dat -> bench name $ nfIO (wf n dat) | (name,wf) <- skipLastBenchs] | n <- samples]
  , bgroup "skip-sum"  [bgroup (show n) [env (genS name n) $ \dat -> bench name $ nfIO (wf n dat) | (name,wf) <- skipSumBenchs ] | n <- samples]
  ]
  where
    genS "noskip" n = fst <$> gen n
    genS "skip" n = do
      removeFile "tree-skip"
      writeMMapInDirectS "tree-skip" n
      return "tree-skip"
    gen n = do
      removeFile "tree"
      writeMMapDirect "tree" n
      bs <- BS.readFile "tree"
      return ("tree",bs)
    verify n f = do
      x <- f
      when (x /= 2^n) $ do
        error $ "wrong output" ++ show x
      pure ()
    writeBenchs =
      [("mmap-direct"    ,writeMMapDirect)
      ,("lazy-direct"    ,writeBSSDirect)
      ,("strict-direct"  ,writeBSSDirect)
      ,("mmap-indirect"  ,writeMMapInDirect)
      ,("lazy-indirect"  ,writeBSSInDirect)
      ,("strict-indirect",writeBSSInDirect)
      ,("binary-indirect",writeBinaryInDirect)
      ,("binary-direct"  ,writeBinaryDirect)
      ]
    sumBenchs =
      [("mmap"   ,sumMMap)
      ,("strict" ,sumBSS)
      ,("lazy"   ,sumBSL)
      ,("binary" ,sumBinary)
      ,("memory" ,sumInMem)
      ,("memory-cursor" ,sumInMemCursor)
      ]
    modifyBenchs =
      [("mmap"  ,add1MMap)
      ,("strict",add1Strict)
      ,("lazy"  ,add1Lazy)
      ,("binary",add1Binary)
      ]
    skipWBenchs =
      [("noskip",writeMMapInDirect)
      ,("skip", writeMMapInDirectS)
      ]
    skipLastBenchs =
      [("noskip",lastMMap)
      ,("skip", lastMMapS)
      ]
    skipSumBenchs =
      [("noskip",\n fp -> sumMMap n (fp, undefined))
      ,("skip", sumMMapS)
      ]

-- Write

-- Direct
writeMMapDirect :: FilePath -> Int -> IO ()
writeMMapDirect fp n =
  unsafeMMapWriteBuffer fp (f n) (\wcur -> writeTreeInPlace n wcur & \wcur -> consumeCursor wcur & \() -> Ur ())

writeBSLDirect :: FilePath -> Int -> IO ()
writeBSLDirect fp n = do
  let Ur (bs, !()) = writeBufferLazy (2^22) (\wcur -> Res () (writeTreeInPlace n wcur))
  BSL.writeFile fp bs

writeBSSDirect :: FilePath -> Int -> IO ()
writeBSSDirect fp n = do
  let Ur (bs,!()) = unsafeWriteBuffer (f n) (\wcur -> writeTreeInPlace n wcur & \wcur -> Res () wcur)
  BS.writeFile fp bs

-- InDirect

writeMMapInDirect :: FilePath -> Int -> IO ()
writeMMapInDirect fp n = do
  unsafeMMapWriteBuffer fp (f n) (\wcur -> writeTree (tree n) wcur & \wcur -> consumeCursor wcur & \() -> Ur ())

writeMMapInDirectS :: FilePath -> Int -> IO ()
writeMMapInDirectS fp n = do
  unsafeMMapWriteBuffer fp (fs n) (\wcur -> writeSTree (stree n) wcur & \wcur -> consumeCursor wcur & \() -> Ur ())

writeBSLInDirect :: FilePath -> Int -> IO ()
writeBSLInDirect fp n = do
  let Ur (bs, !()) = writeBufferLazy (2^22) (\wcur -> Res () (writeTree (tree n) wcur))
  BSL.writeFile fp bs

writeBSSInDirect :: FilePath -> Int -> IO ()
writeBSSInDirect fp n = do
  let t = tree n
      Ur (bs,!()) = unsafeWriteBuffer (f n) (\wcur -> writeTree t wcur & \wcur -> Res () wcur)
  BS.writeFile fp bs

writeBinaryInDirect :: FilePath -> Int -> IO ()
writeBinaryInDirect fp n = encodeFile fp (tree n)

writeBinaryDirect :: FilePath -> Int -> IO ()
writeBinaryDirect fp n = BSL.writeFile fp $ runPut $ putTree n
  where
    putTree 0 = putWord8 0 >> putInthost 1
    putTree n = putWord8 0 >> putTree (n-1) >> putTree (n-1)

-- Sum

sumMMapS :: Int -> FilePath -> IO Int
sumMMapS _ fp = do
  bs <- mmapFileByteString fp Nothing
  pure $ unur (unsafeReadBuffer bs (\c -> sumSTree c & \case
    Res x r -> consumeCursor r & \() -> Ur x))

lastMMapS :: Int -> FilePath -> IO Int
lastMMapS _ fp = do
  bs <- mmapFileByteString fp Nothing
  pure $ unur (unsafeReadBuffer bs (\c -> lastSTree c & \case
    Res x r -> consumeCursor r & \() -> Ur x))

lastMMap :: Int -> FilePath -> IO Int
lastMMap _ fp = do
  bs <- mmapFileByteString fp Nothing
  pure $ unur (unsafeReadBuffer bs (\c -> lastTree c & \case
    Res x r -> consumeCursor r & \() -> Ur x))

sumMMap :: Int -> (FilePath, ByteString) -> IO Int
sumMMap _ (fp,_) = sumBSTree <$> mmapFileByteString fp Nothing

sumBSL :: Int -> (FilePath, ByteString) -> IO Int
sumBSL _ (fp,_) = sumBSTreeL <$> BSL.readFile fp

sumBSS :: Int -> (FilePath, ByteString) -> IO Int
sumBSS _ (fp,_) = sumBSTree <$> BS.readFile fp

sumBinary :: Int -> (FilePath, ByteString) -> IO Int
sumBinary _ (fp,_) = sumTreeSlow <$> decodeFile fp

sumInMem :: Int -> (FilePath, ByteString) -> IO Int
sumInMem n _ = pure $ sumTreeSlow $ tree n

sumInMemCursor :: Int -> (FilePath, ByteString) -> IO Int
sumInMemCursor n (_,bs) = pure $ sumBSTree bs

-- Modify

add1MMap :: Int -> FilePath -> IO ()
add1MMap n fp = do
  bs <- mmapFileByteString fp Nothing
  unsafeMMapWriteBuffer (fp++"1") (f n) $ \wcur ->
    unsafeReadBuffer bs (\rcur ->
      add1 rcur wcur & \(rcur,wcur) ->
        consumeCursor rcur & \() ->
        consumeCursor wcur & \() ->
        Ur ())

add1Strict :: Int -> FilePath -> IO ()
add1Strict n fp = do
  bs <- BS.readFile fp
  let Ur bs' = unsafeReadBuffer bs (\rcur ->
               unsafeWriteBuffer (f n) (\wcur ->
                 add1 rcur wcur & \(rcur,wcur) ->
                   consumeCursor rcur & \() ->
                     Res () wcur)
                 & \(Ur (bs,())) -> Ur bs)
  BS.writeFile (fp++"1") bs'

add1Lazy :: Int -> FilePath -> IO ()
add1Lazy n fp = do
  bs <- BSL.readFile fp
  let Ur bs' = unsafeReadLazyBuffer bs (\rcur ->
                 writeBufferLazy (2^22) (\wcur ->
                   add1 rcur wcur & \(rcur,wcur) ->
                     consumeCursor rcur & \() ->
                       Res () wcur)
                 & \(Ur (bs,())) -> Ur bs)
  BSL.writeFile (fp++"1") bs'

add1Binary :: Int -> FilePath -> IO ()
add1Binary n fp = do
  tree <- decodeFile fp
  encodeFile (fp++"1") (add1Tree tree)

