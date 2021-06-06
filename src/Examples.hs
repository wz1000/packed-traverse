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
{-# LANGUAGE UnboxedTuples #-}

module Examples where

import qualified GHC.Generics as GHC
import Generics.SOP

import Data.Unrestricted.Linear
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BSL
import Prelude.Linear ((&))

import Types
import Cursor
import Class
import Data.Binary

data Tree = Leaf Int | Node Tree Tree
  deriving stock (Show, GHC.Generic, Eq)
  deriving anyclass (Generic, HasDatatypeInfo, Binary)

data SkipTree = LeafS Int | NodeS (Skippable SkipTree) (Skippable SkipTree)
  deriving stock (Show, GHC.Generic, Eq)
  deriving anyclass (Generic, HasDatatypeInfo, Binary)

-- instance Serialize Tree
-- instance Deserialize Tree

data Tree2 = Empty2 | Leaf2 Int | Node2 Int Tree2 Tree2
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo)

lastTree :: RCursor (Tree ': xs) %1 -> Res Int (RCursor xs)
lastTree cur = readTaggedCons cur &
     brCase  #_Leaf readStorable
  <| brCase1 #_Node (\left ->
        lastTree left & \case
          Res _ right -> lastTree right)

lastSTree :: RCursor (SkipTree ': xs) %1 -> Res Int (RCursor xs)
lastSTree cur = readTaggedCons cur &
     brCase  #_LeafS readStorable
  <| brCase1 #_NodeS (\left -> skip left & unSkip & lastSTree)

sumSTree :: RCursor (SkipTree ': xs) %1 -> Res Int (RCursor xs)
sumSTree = go 0
  where
    go :: forall xs. Int -> RCursor (SkipTree ': xs) %1 -> Res Int (RCursor xs)
    go !acc cur = readTaggedCons cur &
           brCase  #_LeafS (\cur -> readStorable cur & \case
                              Res i cur' -> Res (acc+i) cur')
        <| brCase1 #_NodeS (\left -> unSkip left & go acc & \case
              Res acc' right -> unSkip right & go acc')

writeSTree :: SkipTree -> WCursor (SkipTree ': xs) %1 -> WCursor xs
writeSTree (LeafS i  ) cur = writeTaggedCons #_LeafS cur & writeStorable i
writeSTree (NodeS (Skippable l) (Skippable r)) cur
  = writeTaggedCons #_NodeS cur & (\cur -> writeSkippable' (writeSTree l) cur & writeSkippable' (writeSTree r))

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

writeTreeInPlace :: Int -> WCursor (Tree ': xs) %1 -> WCursor xs
writeTreeInPlace 0 cur = writeTaggedCons #_Leaf cur & writeStorable 1
writeTreeInPlace n cur = writeTaggedCons #_Node cur & writeTreeInPlace (n-1) & writeTreeInPlace (n-1)

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
         Res i rcur -> writeTaggedCons #_Leaf wcur & writeStorable i & \wcur -> (rcur, wcur))
  <| brCase1 #_Node (\rcur wcur ->
        writeTaggedCons #_Node wcur & \wcur ->
          copyTree rcur wcur & \(rcur,wcur) ->
            copyTree rcur wcur)

add1 :: RCursor (Tree ': xs) %1 -> WCursor (Tree ': ys) %1 -> (RCursor xs, WCursor ys)
add1 rcur = readTaggedCons rcur &
     brCase #_Leaf (\rcur wcur ->
       readStorable rcur & \case
         Res i rcur -> writeTaggedCons #_Leaf wcur & writeStorable (i+1) & \(!wcur) -> (rcur, wcur))
  <| brCase1 #_Node (\rcur (!wcur) ->
        writeTaggedCons #_Node wcur & \(!wcur) ->
          add1 rcur wcur & \(!rcur,!wcur) ->
            add1 rcur wcur)

add1Tree :: Tree -> Tree
add1Tree (Leaf i) = Leaf (i+1)
add1Tree (Node l r) = Node (add1Tree l) (add1Tree r)

sumTreeSlow :: Tree -> Int
sumTreeSlow = go 0
  where
    go acc (Leaf i) = acc+i
    go !acc (Node l r) = go (go acc l) r

sumBSTree :: ByteString -> Int
sumBSTree bs = unur (unsafeReadBuffer bs (\c -> sumTree c & \case
  Res x r -> consumeCursor r & \() -> Ur x))

sumBSTreeL :: BSL.ByteString -> Int
sumBSTreeL bs = unur (unsafeReadLazyBuffer bs (\c -> sumTree c & \case
  Res x r -> consumeCursor r & \() -> Ur x))

readBSTreeL :: BSL.ByteString -> Tree
readBSTreeL bs = unur (unsafeReadLazyBuffer bs (\c -> readTree c & \case
  Res x r -> consumeCursor r & \() -> Ur x))

tree :: Int -> Tree
tree = go
  where
    go 0 = Leaf 1
    go n = Node (go $ n-1) (go $ n-1)

stree :: Int -> SkipTree
stree = go
  where
    go 0 = LeafS 1
    go n = NodeS (Skippable $ go $ n-1) (Skippable $ go $ n-1)
