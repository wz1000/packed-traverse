{-# LANGUAGE GADTs #-}
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

module Cursor where

import Data.Word

import Generics.SOP

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe
import Data.Unrestricted.Linear
import Data.ByteString.Internal
import Prelude.Linear ((&))
import qualified Unsafe.Linear as Unsafe

import Types
import FastIndex
import Debug.Trace
import System.IO.MMap

{-# INLINE readStorable #-}
readStorable :: forall x xs. Storable x => RCursor (x ': xs) %1 -> Res x (RCursor xs)
readStorable (Cursor ptr) = unsafeDupablePerformIO $ do
  x <- peek (castPtr ptr)
  pure (Res x (Cursor (ptr `plusPtr` sizeOf x)))

{-# INLINE readTaggedCons #-}
readTaggedCons :: forall x xs cs xss. (SListI xss, cs ~ Constructors x, xss ~ Code x) => RCursor (x ': xs) %1 -> Branch cs xss (RStack xs)
readTaggedCons (Cursor ptr) = unsafeDupablePerformIO $ do
  tag <- peek (castPtr ptr)
  let
    ptr'' :: Ptr Word8
    ptr'' = ptr `plusPtr` sizeOf tag
    len = fromIntegral (lengthSList (Proxy @xss))
    msg = "exhausted alternatives in readTag, got " ++ show tag ++ ", expected " ++ show len
  if tag > len
  then error msg
  else pure (UnsafeBranch tag (RStack (Cursor ptr'')))

readTagged :: forall x xs xss. (Code x ~ xss,SListI xss) => RCursor (x ': xs) %1 -> NS (RStack xs) xss
readTagged (Cursor ptr) = unsafeDupablePerformIO $ do
  tag <- peek (castPtr ptr)
  let
    ptr' :: Ptr Word8
    ptr' = ptr `plusPtr` sizeOf tag
    loop :: forall ys. Int -> Shape ys -> NS (RStack xs) ys
    loop _ ShapeNil       = error $ "exhausted alternatives in readTag, got " ++ show tag ++ ", expected " ++ show (lengthSList (Proxy @xss))
    loop 0 (ShapeCons _ ) = Z (RStack (Cursor ptr'))
    loop n (ShapeCons xs) = S (loop (n-1) xs)

  pure $ loop tag shape

consumeCursor :: Cursor t '[] %1 -> ()
consumeCursor (Cursor _) = ()

(<|) :: (a %p -> b) %q -> a %p -> b
(<|) f x = f x

infixr 2 <|

unsafeReadBuffer :: ByteString -> (RCursor xs %1 -> Ur a) %1 -> Ur a
unsafeReadBuffer (PS fp st _) = Unsafe.toLinear (\f -> unsafeDupablePerformIO $ withForeignPtr fp $ \ptr ->
  pure $! case f (Cursor (ptr `plusPtr` st)) of
    Ur x -> Ur x)

writeStorable :: forall x xs. (Storable x, Show x) => x -> WCursor (x ': xs) %1 -> WCursor xs
writeStorable x (Cursor ptr) = unsafeDupablePerformIO $ do
  poke (castPtr ptr) x
  pure (Cursor (ptr `plusPtr` sizeOf x))

writeTagged :: Code x ~ xss => Idx xss ys -> WCursor (x ': xs) %1 -> WCursor (ys ++ xs)
writeTagged idx (Cursor cur) = writeStorable (idxToInt idx) (Cursor cur) & \case
  Cursor ptr -> Cursor ptr

writeTaggedCons :: (cs ~ Constructors x, xss ~ Code x) => IdxB cs xss c ys n -> WCursor (x ': xs) %1 -> WCursor (ys ++ xs)
writeTaggedCons (UnsafeIdxB i) (Cursor cur) = writeStorable i (Cursor cur) & \case
  Cursor ptr -> Cursor ptr

unsafeWriteBuffer :: Int -> (WCursor xs %1 -> Res a (WCursor '[])) %1 -> (ByteString, a)
unsafeWriteBuffer size = Unsafe.toLinear (\f -> unsafeCreateUptoN' size $ \ptr ->
  case f (Cursor ptr) of
    Res a (Cursor ptr') -> do
      pure (ptr' `minusPtr` ptr, a))

unsafeMMapWriteBuffer :: FilePath -> Int -> (WCursor xs %1 -> Ur a) %1 -> IO a
unsafeMMapWriteBuffer fp i = Unsafe.toLinear (\f -> mmapWithFilePtr fp ReadWriteEx (Just (0,i)) $ \(ptr,_) ->
  case f (Cursor (castPtr ptr)) of
    Ur !a -> pure a)

