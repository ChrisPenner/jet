{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import qualified Control.Comonad as Comonad
import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as Cofree
import Control.Lens
import Control.Monad
import Data.Aeson (Value (String))
import qualified Data.Aeson as Aeson
import Data.Aeson.Extra
import qualified Data.ByteString.Lazy as BS
import qualified Data.Functor.Foldable as FF
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graphics.Vty
import qualified Graphics.Vty as Vty
import Pretty
import qualified Zipper as Z
import Control.Monad.State

data FocusState = FS
  { keySelection :: Maybe Text,
    buffer :: Maybe Text
  }

run :: IO ()
run = do
  void $ edit $ fromJust $ Aeson.decode ("[\"hi\", {\"testing\":[1, 2, false, null]}, [4, 5, 6]]")

edit :: Value -> IO Value
edit value = flip evalStateT (FS Nothing Nothing) $ do
  config <- liftIO $ standardIOConfig
  vty <- liftIO $ mkVty config
  let start = fromJust $ (Z.down (Index 0)) . Z.zipper . renderedZipper . toCofree $ value
  let loop foc = do
        liftIO $ update vty . Vty.picForImage $ renderValue' foc
        e <- liftIO $ nextEvent vty
        foc' <- handleEvent foc e
        if (maybeQuit e)
          then pure foc'
          else (loop foc')
  v <- loop start
  liftIO $ Vty.shutdown vty
  pure (Z.flatten v)

maybeQuit :: Vty.Event -> Bool
maybeQuit = \case
  EvKey (KChar 'c') [Vty.MCtrl] -> True
  _ -> False

handleEvent :: MonadState FocusState m => Z.Zipper ValueF a ->  Vty.Event -> m (Z.Zipper ValueF a)
handleEvent z = \case
  EvKey key mods -> case key of
    KChar 'h' -> pure $ Z.tug Z.up z
    KChar 'l' -> into z
    KChar 'j' -> pure $ nextSibling z
    KChar 'k' -> pure $ prevSibling z
    _ -> pure $ z
  _ -> pure $ z

nextSibling :: Z.Zipper ValueF a -> Z.Zipper ValueF a
nextSibling z = fromMaybe z $ do
  parent <- Z.up z
  curI <- Z.currentIndex z
  newI <- case Z.branches parent of
    ObjectF hm -> do
      let keys = HM.keys hm
      (_, newKey) <- List.find ((curI ==) . Key . fst) (zip (HM.keys hm) (drop 1 $ HM.keys hm))
      pure $ Key newKey
    ArrayF _ -> case curI of
      (Index i) -> pure . Index $ i + 1
      _ -> Nothing
    StringF txt -> Nothing
    NumberF sci -> Nothing
    BoolF b -> Nothing
    NullF -> Nothing
  Z.down newI parent

prevSibling :: Z.Zipper ValueF a -> Z.Zipper ValueF a
prevSibling z = fromMaybe z $ do
  parent <- Z.up z
  curI <- Z.currentIndex z
  newI <- case Z.branches parent of
    ObjectF hm -> do
      let keys = HM.keys hm
      (newKey, _) <- List.find ((curI ==) . Key . snd) (zip (HM.keys hm) (drop 1 $ HM.keys hm))
      pure $ Key newKey
    ArrayF _ -> case curI of
      (Index i) -> pure . Index $ i - 1
      _ -> Nothing
    StringF txt -> Nothing
    NumberF sci -> Nothing
    BoolF b -> Nothing
    NullF -> Nothing
  Z.down newI parent

into :: MonadState FocusState m => Z.Zipper ValueF a -> m (Z.Zipper ValueF a)
into z = do
  FS{..}<- get
  case (Z.branches z) of
    (ObjectF hm) -> do
      let fstKey = (HM.keys hm) ^? _head
      case keySelection of
        Nothing -> do
          case fstKey of
            Nothing -> pure z
            Just k -> do
              modify (\fs -> fs{keySelection=Just k})
              pure z
        Just k -> pure $ Z.tug (Z.down (Key k)) z
    ArrayF vec ->
      pure . fromMaybe z $ do
        Z.down (Index 0) z
    StringF txt -> pure z
    NumberF sci -> pure z
    BoolF b -> pure z
    NullF -> pure z

renderValue' :: Z.Zipper ValueF Vty.Image -> Vty.Image
renderValue' z =
  let newA = toImage 2 (colorText (flip Vty.withStyle Vty.reverseVideo)) (z ^. Z.unwrapped . _unwrap . to (fmap Comonad.extract))
   in Z.foldSpine (toImage 2 (colorText id)) $ (z & Z.focus_ .~ newA)

toImage :: Int -> (Maybe Vty.Color -> Text -> Vty.Image) -> ValueF Image -> Image
toImage i img (StringF t) = indentLine i (img (Just green) ("\"" <> t <> "\""))
toImage i img NullF = indentLine i (img (Just Vty.yellow) "null")
toImage i img (NumberF n) = indentLine i (img (Just Vty.blue) (Text.pack $ show n))
toImage i img (BoolF b) = indentLine i (img (Just Vty.magenta) (Text.pack $ show b))
toImage i img (ArrayF xs) = prettyArray i img xs
toImage i img (ObjectF xs) = prettyObj i img xs

colorText :: (Attr -> Attr) -> (Maybe Vty.Color) -> Text -> Vty.Image
colorText mod col txt = Vty.text' (mod $ maybe Vty.defAttr (Vty.withForeColor Vty.defAttr) col) txt <|> colorFix

renderedZipper :: Cofree ValueF x -> Cofree ValueF Image
renderedZipper = Z.retag (toImage 2 (colorText id))

atom :: Text -> Vty.Image
atom t = Vty.text' defAttr t

prettyArray :: Int -> (Maybe Vty.Color -> Text -> Vty.Image) -> Vector Vty.Image -> Vty.Image
prettyArray i img vs =
  let inner :: [Image] = indented 1 (Vector.toList vs)
   in Vty.vertCat . indented i $ [img Nothing "["] ++ inner ++ [img Nothing "]"]

prettyObj :: Int -> (Maybe Vty.Color -> Text -> Vty.Image) -> HashMap Text Vty.Image -> Vty.Image
prettyObj i img vs =
  let inner :: [Image] = indented 2 (HM.foldrWithKey' (\k v a -> [Vty.string (Vty.withForeColor defAttr Vty.cyan) (show k) <|> Vty.text' defAttr ": ", v] ++ a) [] vs)
   in Vty.vertCat . indented i $
        ([img Nothing "{"] ++ inner ++ [img Nothing "}"])

colorFix :: Vty.Image
colorFix = Vty.text' defAttr ""

indented :: Functor f => Int -> f Vty.Image -> f Vty.Image
indented n xs = (indentLine n) <$> xs

indentLine :: Int -> Vty.Image -> Vty.Image
indentLine n x = (Vty.text' defAttr $ Text.replicate n " ") <|> x

-- prettyArray :: Vector (Pretty ()) -> Pretty ()
-- prettyArray vs = do
--   let totalSize = (2 * (Vector.length vs)) + sum (fmap size vs)
--   gets

-- , singleLine = "[" <|> Text.intercalate ", " (Vector.toList $ fmap singleLine vs) <|> "]"
-- , multiLine = "[\n" : concatMap (\p -> ", " : multiLine p) vs ++ ["]"]
-- }

-- prettyImage :: Pretty Text -> Pretty Image
-- prettyImage (Pretty )

project :: Value -> Z.Zipper ValueF ()
project = Z.zipper . asCofree

asCofree :: Value -> Cofree ValueF ()
asCofree = Cofree.unfold (((),) . FF.project)

data JIndex
  = Index Int
  | Key Text
  deriving (Show, Eq, Ord)

instance Z.Idx ValueF where
  type IxOf ValueF = JIndex
  idx :: Z.IxOf ValueF -> Traversal' (ValueF a) a
  idx (Index i) f (ArrayF xs) = ArrayF <$> ix i f xs
  idx (Key k) f (ObjectF xs) = ObjectF <$> ix k f xs
  idx _ _ x = pure x

toCofree :: (Value -> Cofree ValueF ())
toCofree t = Cofree.unfold (\x -> ((), FF.project x)) t
