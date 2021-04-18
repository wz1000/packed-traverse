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
import Data.SOP.Constraint

import Foreign.Ptr

import FastIndex
import GHC.Exts
import GHC.IO
import GHC.IORef
import GHC.Ptr
import Unsafe.Coerce

data Dir = Read | Write

newtype Producer t = Producer (State# RealWorld -> (# State# RealWorld, Addr#, Addr#, Producer t #))
-- type Producer t = IORef (Cursor t Any)

runProducer :: forall xs t. Producer t -> Cursor t xs
runProducer (Producer p) = case runRW# p of
  (# _, start, end, p' #) -> Cursor (Ptr start) (Ptr end) p'

runProducer' :: forall xs t. Producer t -> IO (Cursor t xs)
runProducer' (Producer p) = IO $ \s -> case p s of
  (# s', start, end, p' #) -> (# s', Cursor (Ptr start) (Ptr end) p' #)

createProducer :: (forall xs. IO (Cursor t xs)) -> Producer t
createProducer (IO f) = Producer $ \s -> case f s of
  (# s', Cursor (Ptr start) (Ptr end) p #) -> (# s', start, end, p #)

data Cursor (t :: Dir) (xs :: [Type]) where
  Cursor :: {-# UNPACK #-} !(Ptr Word8) -> {-# UNPACK #-} !(Ptr Word8) -> {-# UNPACK #-} !(Producer t) -> Cursor t xs

absurdNS :: forall a f. NS f '[] %1 -> a
absurdNS = \case{}

type RCursor = Cursor 'Read
type WCursor = Cursor 'Write

newtype RStack (ys :: [Type]) (xs :: [Type]) where
  RStack :: forall ys xs. RCursor (xs ++ ys) %1 -> RStack ys xs

-- | Strict and non linear in a
data Res a b where
  Res :: !a -> b %1 -> Res a b

type family SameShape (xs :: [k1]) (ys :: [k2]) :: Constraint where
  SameShape '[] ys = (ys ~ '[])
  SameShape (x ': xs) ys = (ys ~ (Head ys ': Tail ys), SameShape xs (Tail ys))

{-# INLINE brCase #-}
brCase :: IdxB cs xs c x n -> (RCursor (x ++ ys) %1 -> (a :: TYPE r)) -> (Branch (Delete n cs) (Delete n xs) (RStack ys) %1 -> a) -> Branch cs xs (RStack ys) %1 -> a
brCase i here there b = bCase i (\(RStack c) -> here c) there b

{-# INLINE brCase1 #-}
brCase1 :: IdxB '[c] '[x] c x ZN -> (RCursor (x ++ ys) %1 -> (a :: TYPE r)) -> Branch '[c] '[x] (RStack ys) %1 -> a
brCase1 i here b = bCase1 i (\(RStack c) -> here c) b

