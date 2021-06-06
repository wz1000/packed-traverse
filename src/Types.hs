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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Types where

import Data.Kind
import Data.Word

import Generics.SOP

import Foreign.Ptr

import FastIndex
import GHC.Exts
import GHC.Types
import GHC.IO
import Control.Monad
import GHC.Int

data Dir = Read | Write

data Producer t
  = Producer
  { flushProducer :: {-# UNPACK #-} !(IO ()) -- ^ flush the current buffer
  , getNext :: !(forall xs. Int# -> IO (Cursor t xs))
    -- ^ get the next chunk of the buffer, with at least the given size: Important to flush the buffer before calling this function
  }

emptyProducer :: Producer t
emptyProducer = Producer (pure ()) (\_ -> pure (error "empty producer"))

{-# INLINE runProducer #-}
runProducer :: forall xs t. Producer t -> Cursor t xs
runProducer = unsafePerformIO . runProducer' 0

{-# INLINE runProducer' #-}
runProducer' :: forall xs t. Int64 -> Producer t -> IO (Cursor t xs)
runProducer' (I64# s) p = do
  flushProducer p
  getNext p s

{-# INLINE createProducer #-}
createProducer :: IO () -> (forall xs. Int# -> IO (Cursor t xs)) -> Producer t
createProducer = Producer

data Cursor (t :: Dir) (xs :: [Type]) where
  Cursor :: {-# UNPACK #-} !(Ptr Word8) -> {-# UNPACK #-} !(Ptr Word8) -> {-# NOUNPACK #-} !(Producer t) -> Cursor t xs

absurdNS :: forall a f. NS f '[] %1 -> a
absurdNS = \case{}

type RCursor = Cursor 'Read
type WCursor = Cursor 'Write

newtype RStack (ys :: [Type]) (xs :: [Type]) where
  RStack :: forall ys xs. RCursor (xs ++ ys) %1 -> RStack ys xs

-- | Strict and non linear in a
data Res a b where
  Res :: !a -> b %1 -> Res a b

{-# INLINE brCase #-}
brCase :: IdxB cs xs c x n -> (RCursor (x ++ ys) %1 -> (a :: TYPE r)) -> (Branch (Delete n cs) (Delete n xs) (RStack ys) %1 -> a) -> Branch cs xs (RStack ys) %1 -> a
brCase i here there b = bCase i (\(RStack c) -> here c) there b

{-# INLINE brCase1 #-}
brCase1 :: IdxB '[c] '[x] c x ZN -> (RCursor (x ++ ys) %1 -> (a :: TYPE r)) -> Branch '[c] '[x] (RStack ys) %1 -> a
brCase1 i here b = bCase1 i (\(RStack c) -> here c) b

