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

module Types where

import Data.Kind
import Data.Word

import Generics.SOP
import Data.SOP.Constraint

import Foreign.Ptr

import Index

data Dir = Read | Write

data Cursor (t :: Dir) (xs :: [Type]) where
  Cursor :: !(Ptr Word8) -> Cursor t xs

absurdNS :: forall a f. NS f '[] %1 -> a
absurdNS = \case{}

type RCursor = Cursor 'Read
type WCursor = Cursor 'Write

newtype RStack (ys :: [Type]) (xs :: [Type]) where
  RStack :: forall ys xs. RCursor (xs ++ ys) %1 -> RStack ys xs

-- | Strict and non linear in a
data Res a b where
  Res :: a -> b %1 -> Res a b

type family SameShape (xs :: [k1]) (ys :: [k2]) :: Constraint where
  SameShape '[] ys = (ys ~ '[])
  SameShape (x ': xs) ys = (ys ~ (Head ys ': Tail ys), SameShape xs (Tail ys))

brCase :: IdxB cs xs c x n -> (RCursor (x ++ ys) %p -> a) -> (Branch (Delete n cs) (Delete n xs) (RStack ys) %p -> a) -> Branch cs xs (RStack ys) %p -> a
brCase i here there b = bCase i (\(RStack c) -> here c) there b

brCase1 :: IdxB '[c] '[x] c x ZN -> (RCursor (x ++ ys) %p -> a) -> Branch '[c] '[x] (RStack ys) %p -> a
brCase1 HereB here (This x) = (\(RStack a) -> here a) x

