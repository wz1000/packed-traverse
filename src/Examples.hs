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

module Examples where

import qualified GHC.Generics as GHC
import Generics.SOP

import Data.Unrestricted.Linear
import Data.ByteString.Internal
import Prelude.Linear ((&))

import Types
import Cursor
import Class
import Data.Binary

data Tree = Leaf Int | Node Tree Tree
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, Binary)

-- instance Serialize Tree
-- instance Deserialize Tree

data Tree2 = Empty2 | Leaf2 Int | Node2 Int Tree2 Tree2
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo)

sumTree :: RCursor (Tree ': xs) %1 -> Res Int (RCursor xs)
sumTree = go 0
  where
    go :: forall xs. Int -> RCursor (Tree ': xs) %1 -> Res Int (RCursor xs)
    go !acc cur = readTaggedCons cur &
           brCase  #_Leaf (\cur -> readStorable cur & \case
                              Res i cur' -> Res (acc+i) cur')
        <| brCase1 #_Node (\left ->
              go acc left & \case
                Res acc' right -> go acc' right)

sumTree2 :: RCursor (Tree2 ': xs) %1 -> Res Int (RCursor xs)
sumTree2 = go 0
  where
    go :: forall xs. Int -> RCursor (Tree2 ': xs) %1 -> Res Int (RCursor xs)
    go !acc cur = readTaggedCons cur &
           brCase  #_Empty2 (\cur -> Res 0 cur)
        <| brCase  #_Leaf2  (\cur -> readStorable cur & \case
                               Res i cur' -> Res (acc+i) cur')
        <| brCase1 #_Node2  (\icur ->
             readStorable icur & \case
               Res i left ->
                 go (acc+i) left & \case
                   Res acc' right -> go acc' right)

readTree :: RCursor (Tree ': xs) %1 -> Res Tree (RCursor xs)
readTree cur = readTaggedCons cur &
     brCase  #_Leaf (\cur ->
       readStorable cur & \case
         Res i cur' -> Res (Leaf i) cur')
  <| brCase1 #_Node (\left ->
       readTree left & \case
         Res l right -> readTree right & \case
           Res r end -> Res (Node l r) end)

writeTree :: Tree -> WCursor (Tree ': xs) %1 -> WCursor xs
writeTree (Leaf i  ) cur = writeTaggedCons #_Leaf cur & writeStorable i
writeTree (Node l r) cur = writeTaggedCons #_Node cur & writeTree l & writeTree r

writeTree2 :: Tree2 -> WCursor (Tree2 ': xs) %1 -> WCursor xs
writeTree2 Empty2        cur = writeTaggedCons #_Empty2 cur
writeTree2 (Leaf2 i    ) cur = writeTaggedCons #_Leaf2  cur & writeStorable i
writeTree2 (Node2 i l r) cur = writeTaggedCons #_Node2  cur & writeStorable i & writeTree2 l & writeTree2 r

copyTree :: RCursor (Tree ': xs) %1 -> WCursor (Tree ': xs) %1 -> (RCursor xs, WCursor xs)
copyTree rcur = readTaggedCons rcur &
     brCase #_Leaf (\rcur wcur ->
       readStorable rcur & \case
         Res i rcur -> (rcur, writeTaggedCons #_Leaf wcur & writeStorable i))
  <| brCase1 #_Node (\rcur wcur ->
        writeTaggedCons #_Node wcur & \wcur ->
          copyTree rcur wcur & \(rcur,wcur) ->
            copyTree rcur wcur)

sumTreeSlow :: Tree -> Int
sumTreeSlow = go 0
  where
    go acc (Leaf i) = acc+i
    go !acc (Node l r) = go (go acc l) r

sumBSTree :: ByteString -> Int
sumBSTree bs = unsafeReadBuffer bs $ \c -> sumTree c & \case
  Res x r -> consumeCursor r & \() -> Ur x

tree :: Int -> Tree
tree = go
  where
    go 0 = Leaf 1
    go n = Node (go $ n-1) (go $ n-1)
