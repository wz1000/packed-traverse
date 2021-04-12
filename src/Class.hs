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

module Class where

import Generics.SOP

import Foreign.Storable

import Prelude.Linear ((&))
import Data.Coerce

import Types
import Cursor
import FastIndex

-- class Serialize a where
--   serialize :: a -> WCursor (a ': as) %1 -> WCursor as
--   default serialize :: (Generic a, All2 Serialize (Code a)) => a -> WCursor (a ': as) %1 -> WCursor as
--   serialize a = gserialize (from a)

-- class Deserialize a where
--   deserialize :: RCursor (a ': as) %1 -> Res a (RCursor as)
--   default deserialize :: (Generic a, All2 Deserialize (Code a)) => RCursor (a ': as) %1 -> Res a (RCursor as)
--   deserialize cur = gdeserialize cur & \case
--     Res sop cur -> Res (to $ SOP sop) cur

-- newtype FromStorable a = FromStorable a
-- instance Storable a => Serialize (FromStorable a) where
--   serialize (FromStorable a) = coerce (writeStorable a)
-- instance Storable a => Deserialize (FromStorable a) where
--   deserialize (Cursor ptr) = coerce (readStorable (Cursor ptr :: RCursor (a ': as)))

-- deriving via (FromStorable Int) instance Serialize Int
-- deriving via (FromStorable Int) instance Deserialize Int

-- deriving via (FromStorable Bool) instance Serialize Bool
-- deriving via (FromStorable Bool) instance Deserialize Bool

-- deriving via (FromStorable Char) instance Serialize Char
-- deriving via (FromStorable Char) instance Deserialize Char

-- gserialize :: forall a xs xss. (All2 Serialize xss, xss ~ Code a) => SOP I xss -> WCursor (a ': xs) %1 -> WCursor xs
-- gserialize (SOP sop) = getIdx (Proxy @(All Serialize)) sop go
--   where
--     go :: (All Serialize as) => Idx xss as -> NP I as -> WCursor (a ': xs) %1 -> WCursor xs
--     go idx np cur = writeTagged idx cur & goP np

--     goP :: All Serialize as => NP I as -> WCursor (as ++ xs) %1 -> WCursor xs
--     goP Nil cur = cur
--     goP (I x :* xs) cur = serialize x cur & goP xs

-- gdeserialize :: forall a xs xss. (All2 Deserialize xss, xss ~ Code a, Generic a) => RCursor (a ': xs) %1 -> Res (NS (NP I) xss) (RCursor xs)
-- gdeserialize cur = readTagged cur & go
--   where
--     go :: (All2 Deserialize ass, All SListI ass) => NS (RStack xs) ass %1 -> Res (NS (NP I) ass) (RCursor xs)
--     go (Z (RStack cur)) = goP sList cur & \(Res ps cur) -> Res (Z ps) cur
--     go (S xs) = go xs & \(Res ns cs) -> Res (S ns) cs

--     goP :: (All Deserialize as) => SList as -> RCursor (as ++ ys) %1 -> Res (NP I as) (RCursor ys)
--     goP SNil cur = Res Nil cur
--     goP SCons cur = deserialize cur & \(Res x cur) -> goP sList cur & \(Res xs cur) -> Res (I x :* xs) cur


-- copySerialized :: (Serialize x, Deserialize x) => RCursor (x ': xs) %1 -> WCursor (x ': xs) %1 -> (RCursor xs, WCursor xs)
-- copySerialized rcur wcur = deserialize rcur & \(Res a rcur) -> serialize a wcur & \wcur -> (rcur, wcur)
