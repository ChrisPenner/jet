{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Graphics.Vty
import Data.Aeson (Value (String))
import qualified Zipper as Z
import Data.Aeson.Extra
import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as Cofree
import qualified Data.Functor.Foldable as FF
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import qualified Graphics.Vty as Vty
import Control.Monad

run :: IO ()
run = do
  void $ edit (String "hi")

edit :: Value -> IO Value
edit value = do
  config <- standardIOConfig
  vty <- mkVty config
  let start = Z.tagged (const ()) value
  let loop foc = do
        update vty . Vty.picForImage $ renderValue foc
        e <- nextEvent vty
        loop foc
  loop start

renderValue :: Z.Zipper ValueF a -> Image
renderValue = Z.fold (toImage . snd)
  where
    toImage (StringF t) = Vty.text' mempty ("\"" <> t <> "\"")
    toImage NullF = Vty.text' mempty "null"
    toImage (NumberF n) = Vty.text' mempty (Text.pack $ show n)
    toImage (BoolF b) = Vty.text' mempty (Text.pack $ show b)
    toImage (ArrayF xs) = undefined -- Vty.text' mempty (Text.pack $ show n)
    toImage (ObjectF xs) = undefined -- Vty.text' mempty (Text.pack $ show n)

project :: Value -> Z.Zipper ValueF ()
project = Z.zipper . asCofree

asCofree :: Value -> Cofree ValueF ()
asCofree = Cofree.unfold (((),) . FF.project)


data JIndex =
      ArrI Int
    | ObjI Text
    | Null


instance Z.Idx ValueF where
  type IxOf ValueF = JIndex
  idx :: Z.IxOf ValueF -> Traversal' (ValueF a) a
  idx (ArrI i) f (ArrayF xs) = ArrayF <$> ix i f xs
  idx (ObjI k) f (ObjectF xs) = ObjectF <$> ix k f xs
  idx _ _ x = pure x

