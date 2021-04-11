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

module Index where

import Data.Kind

import Generics.SOP
import qualified Generics.SOP.Type.Metadata as M
import GHC.TypeLits hiding (Nat)
import GHC.OverloadedLabels

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data Branch (cs :: [Symbol]) (xs :: [k]) (f :: k -> Type) where
  This :: !(f x) %1 -> Branch (c ': cs) (x ': xs) f
  That :: !(Branch cs xs f) %1 -> Branch (c ': cs) (x ': xs) f

data Nat = ZN | SN Nat

type family Delete (n :: Nat) (xs :: [k]) :: [k] where
  Delete ZN (x ': xs) = xs
  Delete (SN n) (y ': xs) = y ': (Delete n xs)

data IdxB (cs :: [Symbol]) (xs :: [k]) (c :: Symbol) (x :: k) (n :: Nat) where
  HereB  :: IdxB (c ': cs) (x ': xs) c x ZN
  ThereB :: !(IdxB cs xs c x n) -> IdxB (c1 ': cs) (x1 ': xs) c x (SN n)

class LookupIdxB (cs :: [Symbol]) (xs :: [k]) (c :: Symbol) (x :: k) (n :: Nat) | cs xs c -> x, cs xs c -> n where
  lookupIdxB :: IdxB cs xs c x n

instance (xs ~ (x ':_xs), n ~ ZN) => LookupIdxB (c ': cs) xs c x n where
  lookupIdxB = HereB

instance {-# OVERLAPPABLE #-} (xs ~ (_x ': xs'), n ~ (SN n'), LookupIdxB cs xs' c x n') => LookupIdxB (_c ': cs) xs c x n where
  lookupIdxB = ThereB lookupIdxB

instance (cons' ~ AppendSymbol "_" c, LookupIdxB cs xs c x n) => IsLabel cons' (IdxB cs xs c x n) where
  fromLabel = lookupIdxB

bCase :: IdxB cs xs c x n -> (f x %p -> a) -> (Branch (Delete n cs) (Delete n xs) f %p -> a) -> Branch cs xs f %p -> a
bCase HereB      here  _there (This x) = here x
bCase HereB      _here there  (That x) = there x
bCase (ThereB _) _here there  (This x) = there (This x)
bCase (ThereB i) here  there  (That x) = bCase i here (\b -> there (That b)) x

bCase1 :: IdxB '[c] '[x] c x ZN -> (f x %p -> a) -> Branch '[c] '[x] f %p -> a
bCase1 HereB here (This x) = here x

data Idx (xs :: [k]) (x :: k) where
  Here :: Idx (x ': xs) x
  There :: Idx xs x -> Idx (y ': xs) x

idxToInt :: Idx xs x -> Int
idxToInt = go 0
  where
    go :: Int -> Idx xs x -> Int
    go acc Here = acc
    go !acc (There xs) = go (acc+1) xs

getIdx :: All c xs => Proxy c -> NS f xs -> (forall x. c x => Idx xs x -> f x -> a) -> a
getIdx _ (Z x) f = f Here x
getIdx p (S xs) f = getIdx p xs (\idx x -> f (There idx) x)

data CtorIdx (a :: Type) (fs :: [Type]) where
  CtorIdx :: Code a ~ xss => !(Idx xss fs) -> CtorIdx a fs

class LookupIdx (conss :: [Symbol]) (cons :: Symbol) (xss :: [k]) (fs :: k) | conss cons xss -> fs where
  lookupIdx :: Proxy conss -> Proxy cons -> Idx xss fs

instance (xs ~ (y ':_xs)) => LookupIdx (cons ': conss) cons xs y where
  lookupIdx _ _ = Here

instance {-# OVERLAPPABLE #-} (xs ~ (_x ': xs'), LookupIdx conss cons xs' x) => LookupIdx (_cons ': conss) cons xs x where
  lookupIdx _ _ = There (lookupIdx (Proxy @conss) (Proxy @cons))

type family ConsInfos (x :: M.DatatypeInfo) :: [M.ConstructorInfo] where
  ConsInfos (M.ADT _ _ xs _) = xs
  ConsInfos (M.Newtype _ _ x) = '[x]
type family Constructors' (xs :: [M.ConstructorInfo]) :: [Symbol] where
  Constructors' '[] = '[]
  Constructors' (M.Constructor cons ': xs) = cons : Constructors' xs
  Constructors' (M.Infix cons _ _ ': xs) = cons : Constructors' xs
  Constructors' (M.Record cons _ ': xs) = cons : Constructors' xs
type Constructors x = Constructors' (ConsInfos (DatatypeInfoOf x))

instance ( HasDatatypeInfo a
         , conss ~ Constructors a
         , LookupIdx conss cons (Code a) fs
         , cons' ~ AppendSymbol "_" cons
         ) => IsLabel cons' (CtorIdx a fs) where
  fromLabel = CtorIdx $ lookupIdx (Proxy @(Constructors a)) (Proxy @cons)
