{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module TaggedCofree where

import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Zipper (TextZipper)
import Data.Vector (Vector)

-- | Cofree, but where you know the annotations match the constructor
data TaggedCofree (f :: k -> * -> *) (g :: k -> *) where
  (:@<) :: g k -> f k (TaggedCofree f g) -> TaggedCofree f g

-- | Data kind for tagging constructors and annotations
data JsonK
  = ObjectK
  | ArrayK
  | StringK
  | NumberK
  | BoolK
  | NullK

-- | Json base functor with constructor tag
data TaggedJsonF (k :: JsonK) r where
  ObjectT :: HashMap Text r -> TaggedJsonF 'ObjectK r
  ArrayT :: Vector r -> TaggedJsonF 'ArrayK r
  StringT :: Text -> TaggedJsonF 'StringK r
  NumberT :: Scientific -> TaggedJsonF 'NumberK r
  BoolT :: Bool -> TaggedJsonF 'BoolK r
  NullT :: TaggedJsonF 'NullK r

-- | State required by the structural editor at each node.
data StructuralEditingBuffer (k :: JsonK) where
  ObjectS :: Maybe (Text, TextZipper Text) -> StructuralEditingBuffer 'ObjectK
  ArrayS :: StructuralEditingBuffer 'ArrayK
  StringS :: Text -> StructuralEditingBuffer 'StringK
  NumberS :: TextZipper Text -> StructuralEditingBuffer 'NumberK
  BoolS :: Bool -> StructuralEditingBuffer 'BoolK
  NullS :: StructuralEditingBuffer 'NullK

-- Fold a Tagged Cofree from bottom up with type-indexed state.
fold ::
  (forall k. Functor (f k)) =>
  (forall k. g k -> f k a -> a) ->
  TaggedCofree f g ->
  a
fold f (gk :@< fk) = f gk $ fmap (fold f) fk

map' ::
  (forall k. Functor (f k)) =>
  (forall k. g k -> h k) ->
  TaggedCofree f g ->
  TaggedCofree f h
map' f (gk :@< fk) = (f gk :@< fmap (map' f) fk)

-- ex :: TaggedCofree TaggedJsonF StructuralEditingBuffer
-- ex =
--   ObjectS "key"
--     :@< ObjectT
--       ( HashMap.fromList
--           [ ("a", StringS "" :@< StringT "text")
--           ]
--       )
--
