{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UnboxedTuples #-}

module Cursor where

import Data.Word

import Generics.SOP

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import System.IO.Unsafe
import Data.Unrestricted.Linear
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.ByteString.Lazy as BSL
import Prelude.Linear ((&))
import qualified Unsafe.Linear as Unsafe
import GHC.Exts (touch#)
import GHC.IO

import Types
import FastIndex
import Debug.Trace
import System.IO.MMap
import Data.Bits
import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

{-# INLINE readStorable #-}
readStorable :: forall x xs. Storable x => RCursor (x ': xs) %1 -> Res x (RCursor xs)
readStorable (Cursor ptr end new)
  | rem < size = unsafeDupablePerformIO $ do
      Cursor ptr' end' new' <- runProducer' new
      let slop = size - rem
      x <- allocaBytes size $ \temp -> do
        copyBytes temp ptr rem
        copyBytes (temp `plusPtr` rem) ptr' slop
        peek (castPtr temp)
      pure (Res x (Cursor (ptr' `plusPtr` slop) end' new'))
  | otherwise = unsafeDupablePerformIO $ do
      x <- peek (castPtr ptr)
      pure (Res x (Cursor (ptr `plusPtr` sizeOf x) end new))
  where
    rem = end `minusPtr` ptr
    size = sizeOf (undefined :: x)

{-# INLINE readTaggedCons #-}
readTaggedCons :: forall x xs cs xss. (SListI xss, cs ~ Constructors x, xss ~ Code x) => RCursor (x ': xs) %1 -> Branch cs xss (RStack xs)
readTaggedCons (Cursor ptr end new) = readStorable (Cursor ptr end new) & \(Res tag (Cursor ptr end new)) ->
  if tag >= len
  then error $ "exhausted alternatives in readTag, got " ++ show tag ++ ", expected " ++ show len ++ " at " ++ show ptr
  else UnsafeBranch tag (RStack (Cursor ptr end new))
  where
    len = fromIntegral (lengthSList (Proxy @xss))

consumeCursor :: Cursor t '[] %1 -> ()
consumeCursor (Cursor _ _ _) = ()

(<|) :: (a %p -> b) %q -> a %p -> b
(<|) f x = f x

infixr 2 <|

unsafeReadBuffer :: ByteString -> (RCursor xs %1 -> Ur a) %1 -> Ur a
unsafeReadBuffer (BS fp len) = Unsafe.toLinear (\f -> unsafeDupablePerformIO $ withForeignPtr fp $ \ptr ->
  pure $! case f (Cursor ptr (ptr `plusPtr` len) (createProducer $ pure undefined)) of
    Ur !x -> Ur x)

unsafeReadLazyBuffer :: BSL.ByteString -> (RCursor xs %1 -> Ur a) %1 -> Ur a
unsafeReadLazyBuffer bs f = f (runProducer prod) & \case
    Ur !x -> Ur x
  where
    prod = go (pure ()) bs
    go prev BSL.Empty = createProducer $ do
      prev
      pure $ error "tried to read from empty bytestring"
    go prev (BSL.Chunk (BS fp len) xs) = createProducer $ do
        prev
        let ptr = unsafeForeignPtrToPtr fp
            finalize = touchForeignPtr fp
        pure (Cursor ptr (ptr `plusPtr` len) (go finalize xs))

{-# INLINE writeStorable #-}
writeStorable :: forall x xs. (Storable x, Show x) => x -> WCursor (x ': xs) %1 -> WCursor xs
writeStorable x (Cursor ptr end new)
  | rem < size = unsafeDupablePerformIO $ do
      Cursor ptr' end' new' <- runProducer' new
      let slop = size - rem
      allocaBytes size $ \temp -> do
        poke (castPtr temp) x
        copyBytes ptr temp rem
        copyBytes ptr' (temp `plusPtr` rem) slop
      pure (Cursor (ptr' `plusPtr` slop) end' new')

  | otherwise = unsafeDupablePerformIO $ do
      poke (castPtr ptr) x
      pure (Cursor (ptr `plusPtr` size) end new)
  where
    rem = end `minusPtr` ptr
    size = sizeOf x


{-# INLINE writeTaggedCons #-}
writeTaggedCons :: (cs ~ Constructors x, xss ~ Code x) => IdxB cs xss c ys n -> WCursor (x ': xs) %1 -> WCursor (ys ++ xs)
writeTaggedCons (UnsafeIdxB i) (Cursor cur end new) = writeStorable i (Cursor cur end new) & \case
  Cursor ptr end new -> Cursor ptr end new

{-# INLINE unsafeWriteBuffer #-}
unsafeWriteBuffer :: Int -> (WCursor xs %1 -> Res a (WCursor '[])) %1 -> (ByteString, a)
unsafeWriteBuffer size = Unsafe.toLinear (\f -> unsafeCreateUptoN' size $ \ptr ->
  case f (Cursor ptr (ptr `plusPtr` size) (createProducer $ pure undefined)) of
    Res a (Cursor ptr' _ _) -> do
      pure (ptr' `minusPtr` ptr, a))

{-# NOINLINE unsafeWriteBufferChunked #-}
unsafeWriteBufferChunked :: Int -> (ByteString -> IO ()) -> (WCursor xs %1 -> (Ur a, WCursor '[])) -> IO a
unsafeWriteBufferChunked chunksize k f = do
  let prod prev = createProducer $ do
        prev
        fp <- mallocByteString chunksize
        let ptr = unsafeForeignPtrToPtr fp
            ptr' = ptr `plusPtr` chunksize
            prev' = k (BS fp chunksize)
        pure (Cursor ptr ptr' (prod prev'))
  case f (runProducer (prod (pure ()))) of
    (Ur x,Cursor ptr end prod') -> do
      when (ptr < end) $ do
        let start = end `plusPtr` (negate chunksize)
            size = ptr `minusPtr` start
        last <- create size (\dest -> copyBytes dest start size)
        k last
        IO (\s -> case touch# prod' s of s' -> (# s', () #))
      pure x

data Proc a = Chunk ByteString | Done a

{-# NOINLINE writeBufferLazy #-}
writeBufferLazy :: Int -> (WCursor xs %1 -> (Ur a, WCursor '[])) -> (BSL.ByteString, a)
writeBufferLazy n f = unsafePerformIO $ do
  chan <- newEmptyMVar
  _ <- forkIO $ do
    a <- unsafeWriteBufferChunked n (putMVar chan . Chunk) f
    putMVar chan (Done a)
  go chan
  where
    go chan = do
      x <- takeMVar chan
      case x of
        Done a -> pure (BSL.Empty,a)
        Chunk bs -> do
          ~(rest,a) <- unsafeInterleaveIO (go chan)
          pure (BSL.Chunk bs rest, a)

unsafeMMapWriteBuffer :: FilePath -> Int -> (WCursor xs %1 -> Ur a) %1 -> IO a
unsafeMMapWriteBuffer fp i = Unsafe.toLinear (\f -> mmapWithFilePtr fp ReadWriteEx (Just (0,i)) $ \(ptr,len) ->
  case f (Cursor (castPtr ptr) (ptr `plusPtr` len) (createProducer $ pure undefined)) of
    Ur !a -> pure a)

