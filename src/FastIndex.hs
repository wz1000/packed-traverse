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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module FastIndex where

import Data.Kind
import Data.Word

import Generics.SOP
import qualified Generics.SOP.Type.Metadata as M
import GHC.TypeLits hiding (Nat)
import GHC.OverloadedLabels
import qualified Unsafe.Linear as Unsafe
import GHC.Exts

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data Branch (cs :: [Symbol]) (xs :: [k]) (f :: k -> Type) where
  UnsafeBranch :: !Word8 -> !(f x) %1 -> Branch cs xs f
  -- This :: !(f x) %1 -> Branch (c ': cs) (x ': xs) f
  -- That :: !(Branch cs xs f) %1 -> Branch (c ': cs) (x ': xs) f

data Nat = ZN | SN Nat

type family Delete (n :: Nat) (xs :: [k]) :: [k] where
  Delete ZN (x ': xs) = xs
  Delete (SN n) (y ': xs) = y ': (Delete n xs)

newtype IdxB (cs :: [Symbol]) (xs :: [k]) (c :: Symbol) (x :: k) (n :: Nat) where
  UnsafeIdxB :: Word8 %1 -> IdxB cs xs c x n

class LookupIdxB (cs :: [Symbol]) (xs :: [k]) (c :: Symbol) (x :: k) (n :: Nat) | cs xs c -> x, cs xs c -> n where
  lookupIdxB :: IdxB cs xs c x n

instance (xs ~ (x ':_xs), n ~ ZN) => LookupIdxB (c ': cs) xs c x n where
  lookupIdxB = UnsafeIdxB 0

instance {-# OVERLAPPABLE #-} (xs ~ (_x ': xs'), n ~ (SN n'), LookupIdxB cs xs' c x n') => LookupIdxB (_c ': cs) xs c x n where
  lookupIdxB = case (lookupIdxB :: IdxB cs xs' c x n') of
    UnsafeIdxB i -> UnsafeIdxB (i+1)

instance (cons' ~ AppendSymbol "_" c, LookupIdxB cs xs c x n) => IsLabel cons' (IdxB cs xs c x n) where
  fromLabel = lookupIdxB

{-# INLINE bCase #-}
bCase :: IdxB cs xs c x n -> (f x %1 -> (a :: TYPE r)) -> (Branch (Delete n cs) (Delete n xs) f %1 -> a) -> Branch cs xs f %1 -> a
bCase (UnsafeIdxB i) here there (UnsafeBranch j x) = case compare i j of
  EQ -> here (Unsafe.coerce x)
  GT -> there (UnsafeBranch j x)
  LT -> there (UnsafeBranch (j-1) x)

{-# INLINE bCase1 #-}
bCase1 :: IdxB '[c] '[x] c x ZN -> (f x %1 -> (a :: TYPE r)) -> Branch '[c] '[x] f %1 -> a
bCase1 (UnsafeIdxB 0) here (UnsafeBranch 0 x) = here (Unsafe.coerce x)
bCase1 _ _ x = error "invalid branch" x

type family ConsInfos (x :: M.DatatypeInfo) :: [M.ConstructorInfo] where
  ConsInfos (M.ADT _ _ xs _) = xs
  ConsInfos (M.Newtype _ _ x) = '[x]
type family Constructors' (xs :: [M.ConstructorInfo]) :: [Symbol] where
  Constructors' '[] = '[]
  Constructors' (M.Constructor cons ': xs) = cons : Constructors' xs
  Constructors' (M.Infix cons _ _ ': xs) = cons : Constructors' xs
  Constructors' (M.Record cons _ ': xs) = cons : Constructors' xs
type Constructors x = Constructors' (ConsInfos (DatatypeInfoOf x))
