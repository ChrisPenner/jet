{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import qualified Control.Comonad as Comonad
import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as Cofree
import Control.Lens hiding ((:<))
import Control.Monad
import Control.Monad.State
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
import Data.Text.Zipper as TZ
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graphics.Vty
import qualified Graphics.Vty as Vty
import Text.Read (readMaybe)
import qualified Zipper as Z
import Data.Aeson.Encode.Pretty (encodePretty)

type Buffer = TZ.TextZipper Text


data NodeState =
  ObjKey {_selectedKey :: Text}
    | NoneState

data FocusState = FS
  { _rendered :: Vty.Image
  , _buffer :: TZ.TextZipper Text
  , _nodeState :: NodeState
  }

makeLenses ''FocusState
makeLenses ''NodeState

-- Traversal into the buffer that also updates the rendered images on the focus.
buffer_ :: Traversal' (Z.Zipper ValueF FocusState) (TZ.TextZipper Text)
buffer_ f z =
  case z ^? Z.focus_ . buffer of
    Nothing -> pure z
    Just buf -> f buf <&> \after ->
                  if buf == after then z
                                  else rerenderFocus id (z & Z.focus_ . buffer .~ after)

selectedKey_ :: Traversal' (Z.Zipper ValueF FocusState) Text
selectedKey_ = Z.focus_ . nodeState . selectedKey

nodeState_ :: Traversal' (Z.Zipper ValueF FocusState) NodeState
nodeState_ = Z.focus_ . nodeState


run :: IO ()
run = do
  result <- edit $ fromJust $ Aeson.decode ("[\"hi\", {\"testing\":[1, 2, false, null]}, [4, 5, 6]]")
  BS.putStrLn $ encodePretty result

edit :: Value -> IO Value
edit value = do
  config <- liftIO $ standardIOConfig
  vty <- liftIO $ mkVty config
  let start = fromJust $ (Z.down (Index 0)) . Z.zipper . toCofree $ value
  let loop mode foc = do
        liftIO $ update vty . Vty.picForImage $ renderValue' foc
        e <- liftIO $ nextEvent vty
        let (mode', foc') = handleEvent mode foc e
        if (maybeQuit e)
          then pure foc'
          else (loop mode' foc')
  v <- loop Move start
  liftIO $ Vty.shutdown vty
  pure (Z.flatten v)

maybeQuit :: Vty.Event -> Bool
maybeQuit = \case
  EvKey (KChar 'c') [Vty.MCtrl] -> True
  _ -> False

bufferText :: Buffer -> Text
bufferText = Text.concat . TZ.getText

updateValF :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
updateValF z =
  let txt = z ^. buffer_ . to bufferText
   in z & Z.unwrapped_ . _unwrap
        %~ ( \case
               o@(ObjectF hm) -> o
               a@(ArrayF vec) -> a
               StringF _ -> StringF txt
               (NumberF n) -> NumberF . fromMaybe n . readMaybe $ Text.unpack txt
               BoolF b -> BoolF . fromMaybe b . readMaybe $ Text.unpack txt
               NullF -> NullF
           )
        & rerenderFocus id

valueFText :: Traversal' (ValueF x) Text
valueFText f = \case
  v@(ObjectF hm) -> pure v
  v@(ArrayF vec) -> pure v
  StringF txt -> StringF <$> f txt
  v@(NumberF sci) -> f (Text.pack . show $ sci) <&> \n -> case readMaybe (Text.unpack n) of
    Nothing -> v
    Just n' -> NumberF n'
  v@(BoolF b) -> f (Text.pack . show $ b) <&> \b' -> case readMaybe (Text.unpack b') of
    Nothing -> v
    Just b'' -> BoolF b''
  NullF -> pure NullF

data ZMode = Edit | Move

handleEvent :: ZMode -> Z.Zipper ValueF FocusState -> Vty.Event -> (ZMode, Z.Zipper ValueF FocusState)
handleEvent mode z e =
  case mode of
    Edit ->
      case e of
        EvKey key mods ->
          case key of
            KChar c -> (mode, z & buffer_ %~ TZ.insertChar c)
            KEsc -> do
              (Move, updateValF z)
            _ -> (mode, z)
        _ -> (mode, z)
    Move ->
      case e of
        EvKey key mods -> case key of
          KChar 'h' -> (mode, Z.tug Z.up z)
          KChar 'l' -> (mode, into z)
          KChar 'j' -> (mode, nextSibling z)
          KChar 'k' -> (mode, prevSibling z)
          KChar 'i' -> (Edit, z)
          _ -> (mode, z)
        _ -> (mode, z)

-- editor :: Vty.Event -> Buffer -> Maybe Buffer
-- editor e z =
--   case e of
--     EvKey key mods -> case key of
--       KEsc -> pure $ Z.tug Z.up z
--       KChar 'h' -> pure $ Z.tug Z.up z

nextSibling :: Z.Zipper ValueF a -> Z.Zipper ValueF a
nextSibling z = fromMaybe z $ do
  parent <- Z.up z
  curI <- Z.currentIndex z
  let newI = case Z.branches parent of
        ObjectF hm -> do
          let keys = HM.keys hm
          (_, newKey) <- List.find ((curI ==) . Key . fst) (zip (HM.keys hm) (drop 1 $ HM.keys hm))
          pure $ Key newKey
        ArrayF xs -> case curI of
          (Index i) | i < (length xs - 1) -> pure . Index $ i + 1
          _ -> Nothing
        StringF txt -> Nothing
        NumberF sci -> Nothing
        BoolF b -> Nothing
        NullF -> Nothing
  case newI of
    Just i -> Z.down i parent
    Nothing -> do
      next <- nextSibling <$> Z.up z
      fstI <- getFirstIdx (Z.branches next)
      Z.down fstI next

prevSibling :: Z.Zipper ValueF a -> Z.Zipper ValueF a
prevSibling z = fromMaybe z $ do
  parent <- Z.up z
  curI <- Z.currentIndex z
  let newI = case Z.branches parent of
        ObjectF hm -> do
          let keys = HM.keys hm
          (newKey, _) <- List.find ((curI ==) . Key . snd) (zip (HM.keys hm) (drop 1 $ HM.keys hm))
          pure $ Key newKey
        ArrayF _ -> case curI of
          (Index 0) -> Nothing
          (Index i) -> pure . Index $ i - 1
          _ -> Nothing
        StringF txt -> Nothing
        NumberF sci -> Nothing
        BoolF b -> Nothing
        NullF -> Nothing
  case newI of
    Just i -> Z.down i parent
    Nothing -> do
      prev <- prevSibling <$> Z.up z
      lastI <- getLastIdx (Z.branches prev)
      Z.down lastI prev

getFirstIdx :: ValueF x -> Maybe JIndex
getFirstIdx = \case
  ObjectF hm -> Key . fst <$> uncons (HM.keys hm)
  ArrayF Empty -> Nothing
  ArrayF vec -> Just $ Index 0
  StringF txt -> Nothing
  NumberF sci -> Nothing
  BoolF b -> Nothing
  NullF -> Nothing

getLastIdx :: ValueF x -> Maybe JIndex
getLastIdx = \case
  ObjectF hm -> Key . snd <$> unsnoc (HM.keys hm)
  ArrayF Empty -> Nothing
  ArrayF vec -> Just $ Index (length vec - 1)
  StringF txt -> Nothing
  NumberF sci -> Nothing
  BoolF b -> Nothing
  NullF -> Nothing

newBuffer :: Text -> Buffer
newBuffer txt = TZ.gotoEOF $ TZ.textZipper (Text.lines txt) Nothing

into :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
into z =
  case (Z.branches z) of
    (ObjectF hm) -> do
      let fstKey = (HM.keys hm) ^? _head
      case z ^? selectedKey_ of
        Nothing -> do
          case fstKey of
            Nothing -> z
            Just k -> z & nodeState_ .~ ObjKey k
        Just k -> Z.tug (Z.down (Key k)) z
    ArrayF vec ->
      fromMaybe z $ do
        Z.down (Index 0) z
    StringF txt -> z
    NumberF sci -> z
    BoolF b -> z
    NullF -> z

renderValue' :: Z.Zipper ValueF FocusState -> Image
renderValue' z =
  Z.foldSpine alg $ (_rendered <$> rerenderFocus (flip Vty.withStyle Vty.reverseVideo) z)
    where
      alg :: Image -> ValueF Image -> Image
      alg img = \case
        ObjectF hm -> prettyObj 2 (colorText id) hm
        ArrayF vec -> prettyArray 2 (colorText id) vec
        StringF txt -> img
        NumberF sci -> img
        BoolF b -> img
        NullF -> img

rerenderFocus :: (Attr -> Attr) -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
rerenderFocus mod z = z & Z.focus_ . rendered .~ renderSubtree mod (z ^. Z.unwrapped_)

renderSubtree :: (Attr -> Attr) -> Cofree ValueF FocusState -> Vty.Image
renderSubtree mod z = case z ^. Cofree._unwrap of
  StringF t -> indentLine i (img (Just green) ("\"" <> fromMaybe t mTxt <> "\""))
  NullF -> indentLine i (img (Just Vty.yellow) "null")
  (NumberF n) -> indentLine i (img (Just Vty.blue) $ fromMaybe (Text.pack . show $ n) mTxt)
  (BoolF b) -> indentLine i (img (Just Vty.magenta) (Text.pack . show $ b))
  (ArrayF xs) -> prettyArray i img (renderChildren xs)
  (ObjectF xs) -> prettyObj i img (renderChildren xs)
  where
    mTxt :: (Maybe Text)
    mTxt = z ^? Cofree._extract . buffer . to bufferText
    renderChildren :: (Functor g, Functor f) => f (Cofree g FocusState) -> f Vty.Image
    renderChildren = fmap (_rendered . Comonad.extract)
    img = colorText mod
    i = 2

colorText :: (Attr -> Attr) -> (Maybe Vty.Color) -> Text -> Vty.Image
colorText mod col txt = Vty.text' (mod $ maybe Vty.defAttr (Vty.withForeColor Vty.defAttr) col) txt <|> colorFix

bufferFrom :: ValueF FocusState -> TextZipper Text
bufferFrom = \case
  ObjectF hm -> TZ.textZipper [] Nothing
  ArrayF vec -> TZ.textZipper [] Nothing
  StringF txt -> TZ.textZipper (Text.lines txt) Nothing
  NumberF n -> TZ.textZipper [Text.pack $ show n] Nothing
  BoolF b -> TZ.textZipper [Text.pack $ show b] Nothing
  NullF -> TZ.textZipper [Text.pack $ "null"] Nothing

atom :: Text -> Vty.Image
atom t = Vty.text' defAttr t

prettyArray :: Int -> (Maybe Vty.Color -> Text -> Vty.Image) -> Vector Image -> Vty.Image
prettyArray i img vs =
  let inner :: [Image] = indented 2 (Vector.toList vs)
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
indentLine n x = (Vty.text' (Vty.withForeColor defAttr Vty.brightBlack) $ " " <> Text.replicate (n - 1) " ") <|> x

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

toCofree :: (Value -> Cofree ValueF FocusState)
toCofree t = go t
  where
    go x =
      let rec = fmap toCofree (FF.project x)
          buf = bufferFromValueF $ FF.project x
          img = renderSubtree id (FS mempty buf NoneState :< rec)
          fs = FS img buf NoneState
       in fs :< rec

bufferFromValueF :: ValueF x -> Buffer
bufferFromValueF v = newBuffer $ v ^. valueFText
