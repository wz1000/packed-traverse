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

import Types
import Index

readStorable :: forall x xs. Storable x => RCursor (x ': xs) %1 -> Res x (RCursor xs)
readStorable (Cursor ptr) = unsafePerformIO $ do
  let ptr' = alignPtr (castPtr ptr) (alignment (undefined :: x))
  x <- peek ptr'
  pure (Res x (Cursor (ptr' `plusPtr` sizeOf x)))

readTaggedCons :: forall x xs cs xss. (Generic x, HasDatatypeInfo x, cs ~ Constructors x, xss ~ Code x, xss `SameShape` cs, SListI cs) => RCursor (x ': xs) %1 -> Branch cs xss (RStack xs)
readTaggedCons (Cursor ptr)= unsafePerformIO $ do
  let ptr' = alignPtr (castPtr ptr) (alignment (undefined :: Int))
  tag <- peek ptr'
  let
    ptr'' :: Ptr Word8
    ptr'' = ptr `plusPtr` sizeOf tag

    msg = "exhausted alternatives in readTag, got " ++ show tag ++ ", expected " ++ show (lengthSList (Proxy @xss))

    loop :: forall ys cs. (SameShape ys cs) => Int -> Shape ys -> Shape cs -> Branch cs ys (RStack xs)
    loop _ ShapeNil       ShapeNil       = error msg
    loop 0 (ShapeCons _ ) (ShapeCons _ ) = This (RStack (Cursor ptr''))
    loop n (ShapeCons xs) (ShapeCons cs) = That (loop (n-1) xs cs)

  pure $ loop tag shape shape

readTagged :: forall x xs xss. (Code x ~ xss,SListI xss) => RCursor (x ': xs) %1 -> NS (RStack xs) xss
readTagged (Cursor ptr)= unsafePerformIO $ do
  let ptr' = alignPtr (castPtr ptr) (alignment (undefined :: Int))
  tag <- peek ptr'
  let
    ptr'' :: Ptr Word8
    ptr'' = ptr `plusPtr` sizeOf tag
    loop :: forall ys. Int -> Shape ys -> NS (RStack xs) ys
    loop _ ShapeNil       = error $ "exhausted alternatives in readTag, got " ++ show tag ++ ", expected " ++ show (lengthSList (Proxy @xss))
    loop 0 (ShapeCons _ ) = Z (RStack (Cursor ptr''))
    loop n (ShapeCons xs) = S (loop (n-1) xs)

  pure $ loop tag shape

consumeCursor :: Cursor t '[] %1 -> ()
consumeCursor (Cursor _) = ()

(<|) :: (a %p -> b) %q -> a %p -> b
(<|) f x = f x

infixr 2 <|

unsafeReadBuffer :: ByteString -> (RCursor xs %1 -> Ur a) -> a
unsafeReadBuffer (PS fp st _) f = unsafePerformIO $ withForeignPtr fp $ \ptr ->
  pure $! case f (Cursor (ptr `plusPtr` st)) of
    Ur x -> x

unsafeReadBufferWith :: ByteString -> Cursor t xs %1 -> (Cursor t xs, RCursor ys)
unsafeReadBufferWith (PS fp st _) (Cursor cur) = unsafePerformIO $ withForeignPtr fp $ \ptr ->
  pure $! (Cursor cur, Cursor (ptr `plusPtr` st))

writeStorable :: forall x xs. Storable x => x -> WCursor (x ': xs) %1 -> WCursor xs
writeStorable x (Cursor ptr) = unsafePerformIO $ do
  let ptr' = alignPtr (castPtr ptr) (alignment x)
  poke ptr' x
  pure (Cursor (ptr' `plusPtr` sizeOf x))

writeTagged :: Code x ~ xss => Idx xss ys -> WCursor (x ': xs) %1 -> WCursor (ys ++ xs)
writeTagged idx (Cursor cur) = writeStorable (idxToInt idx) (Cursor cur) & \case
  Cursor ptr -> Cursor ptr

writeTaggedCons :: CtorIdx x ys -> WCursor (x ': xs) %1 -> WCursor (ys ++ xs)
writeTaggedCons (CtorIdx idx) cur = writeTagged idx cur

unsafeWriteBuffer :: Int -> (WCursor xs %1 -> Res a (WCursor '[])) -> (ByteString, a)
unsafeWriteBuffer size f = unsafeCreateUptoN' size $ \ptr ->
  case f (Cursor ptr) of
    Res a (Cursor ptr') -> do
      pure (ptr' `minusPtr` ptr, a)

