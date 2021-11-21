{-# LANGUAGE ApplicativeDo #-}
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

module Lib (run) where

import qualified Control.Comonad as Comonad
import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as Cofree
import Control.Lens hiding ((:<))
import Control.Monad.State
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Extra
import Data.Bifunctor (Bifunctor (second))
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Functor.Foldable as FF
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Zipper as TZ
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graphics.Vty
import qualified Graphics.Vty as Vty
import Text.Read (readMaybe)
import qualified Zipper as Z
import System.Environment (getArgs)
import System.Exit (exitFailure)

type Buffer = TZ.TextZipper Text

data FocusState = FS
  { _rendered :: Vty.Image,
    _buffer :: Buffer,
    _selectedKey :: Maybe Text
  }

makeLenses ''FocusState

focusState_ :: Lens' (Z.Zipper ValueF FocusState) FocusState
focusState_ = Z.focus_

-- Traversal into the buffer that also updates the rendered images on the focus.
buffer_ :: RenderState -> Traversal' (Z.Zipper ValueF FocusState) (TZ.TextZipper Text)
buffer_ rs f z =
  case z ^? Z.focus_ . buffer of
    Nothing -> pure z
    Just buf ->
      f buf <&> \after ->
        if buf == after
          then z
          else rerenderFocus rs (z & Z.focus_ . buffer .~ after)

selectedKey_ :: Lens' (Z.Zipper ValueF FocusState) (Maybe Text)
selectedKey_ f z =
  z & Z.focus_ %%~ \fs -> do
    f (_selectedKey fs) <&> \newKey -> fs {_selectedKey = newKey, _buffer = maybe (_buffer fs) newBuffer newKey}

run :: IO ()
run = do
  json <- getArgs >>= \case
    -- [] -> Aeson.decode . BS.pack <$> getContents
    [f] -> Aeson.decodeFileStrict f
    _ -> exitFailure
  result <- edit $ fromJust $ json
  -- result <- edit $ fromJust $ Aeson.decode ("[\"hi\", {\"testing\":[1, 2, false, null], \"other\":true}, [4, 5, 6]]")
  BS.putStrLn $ encodePretty result

edit :: Value -> IO Value
edit value = do
  config <- liftIO $ standardIOConfig
  vty <- liftIO $ mkVty config
  let start = Z.zipper . toCofree $ value
  let loop mode prevZ z = do
        liftIO $ update vty . Vty.picForImage $ renderValue' (Focused, mode) z
        e <- liftIO $ nextEvent vty
        let (mode', foc') = handleEvent mode prevZ z e & second (rerenderFocus (Focused, mode))
        if (maybeQuit e)
          then pure foc'
          else (loop mode' z foc')
  v <- loop Move start start
  liftIO $ Vty.shutdown vty
  pure (Z.flatten v)

maybeQuit :: Vty.Event -> Bool
maybeQuit = \case
  EvKey (KChar 'c') [Vty.MCtrl] -> True
  EvKey (KChar 'q') [] -> True
  _ -> False

bufferText :: Buffer -> Text
bufferText = Text.concat . TZ.getText

bufferText_ :: Getter (Z.Zipper ValueF FocusState) Text
bufferText_ = Z.focus_ . buffer . to bufferText

updateValF :: RenderState -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
updateValF rs z =
  let txt = z ^. buffer_ rs . to bufferText
   in z & Z.unwrapped_ . _unwrap
        %~ ( \case
               o@(ObjectF hm)
                 | Just k <- z ^. selectedKey_ -> ObjectF $ renameKey k txt hm
                 | otherwise -> o
               a@(ArrayF _vec) -> a
               StringF _ -> StringF txt
               (NumberF n) -> NumberF . fromMaybe n . readMaybe $ Text.unpack txt
               BoolF b -> BoolF . fromMaybe b . readMaybe $ Text.unpack txt
               NullF -> NullF
           )
        & selectedKey_ . _Just .~ txt
        & rerenderFocus rs

renameKey :: (Hashable k, Eq k) => k -> k -> HashMap k v -> HashMap k v
renameKey srcKey destKey hm =
  hm
    &~ do
      v <- use (at srcKey)
      at srcKey .= Nothing
      at destKey .= v

valueFText :: Traversal' (ValueF x) Text
valueFText f = \case
  v@(ObjectF _hm) -> pure v
  v@(ArrayF _vec) -> pure v
  StringF txt -> StringF <$> f txt
  v@(NumberF sci) ->
    f (Text.pack . show $ sci) <&> \n -> case readMaybe (Text.unpack n) of
      Nothing -> v
      Just n' -> NumberF n'
  (BoolF b) -> pure (BoolF b)
  -- f (Text.pack . show $ b) <&> \b' -> case readMaybe (Text.unpack b') of
  --   Nothing -> v
  --   Just b'' -> BoolF b''
  NullF -> pure NullF

boolText_ :: Prism' Text Bool
boolText_ = prism' toText toBool
  where
    toText True = "true"
    toText False = "false"
    toBool "true" = Just True
    toBool "false" = Just False
    toBool _ = Nothing

data ZMode = Edit | Move

handleEvent :: ZMode -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState -> Vty.Event -> (ZMode, Z.Zipper ValueF FocusState)
handleEvent mode prevZ z e =
  case mode of
    Edit ->
      case e of
        EvKey key [] ->
          case key of
            KChar c -> (mode, z & buffer_ (Focused, mode) %~ TZ.insertChar c)
            KLeft -> (mode, z & buffer_ (Focused, mode) %~ TZ.moveLeft)
            KRight -> (mode, z & buffer_ (Focused, mode) %~ TZ.moveRight)
            KBS -> (mode, z & buffer_ (Focused, mode) %~ TZ.deletePrevChar)
            KEsc -> do
              (Move, z & updateValF (Focused, Move) & \z -> z & buffer_ (Focused, Move) .~ bufferFromValueF (Z.branches z))
            _ -> (mode, z)
        _ -> (mode, z)
    Move ->
      case e of
        EvKey key _mods -> case key of
          KChar 'h' -> (mode, z & rerenderFocus (NotFocused, mode) & outOf)
          KChar 'l' -> (mode, z & rerenderFocus (NotFocused, mode) & into)
          KChar 'j' -> (mode, z & rerenderFocus (NotFocused, mode) & nextSibling)
          KChar 'k' -> (mode, z & rerenderFocus (NotFocused, mode) & prevSibling)
          KChar 'i' | canEdit z -> (Edit, z)
          KChar 'b' -> (mode, z & setFocus (BoolF True))
          KChar 'o' -> (mode, z & setFocus (ObjectF mempty))
          KChar 'a' -> (mode, z & setFocus (ArrayF mempty))
          KChar 'n' -> (mode, z & setFocus (NumberF 0))
          KChar 's' -> (mode, z & setFocus (StringF ""))
          KChar 't' -> (mode, z & setFocus (StringF ""))
          KChar 'u' -> (mode, prevZ)
          KChar ' ' -> (mode, z & tryToggle)
          _ -> (mode, z)
        _ -> (mode, z)

setFocus :: ValueF (Cofree ValueF FocusState) -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
setFocus f z = z & Z.branches_ .~ f & buffer_ (Focused, Move) .~ newBuffer ""

tryToggle :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
tryToggle z = z & Z.branches_ %~ \case
   BoolF b -> BoolF (not b)
   x -> x

canEdit :: Z.Zipper ValueF FocusState -> Bool
canEdit z = case Z.branches z of
  StringF {} -> True
  NumberF {} -> True
  ObjectF {}
    | Just _ <- z ^. selectedKey_ -> True
    | otherwise -> False
  ArrayF {} -> False
  BoolF {} -> False
  NullF -> False

nextSibling :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
nextSibling z = fromMaybe z $ do
  case (Z.branches z, z ^. selectedKey_) of
    (ObjectF hm, Just k) -> do
      prevKey <- nextInList k $ HashMap.keys hm
      pure $ z & selectedKey_ ?~ prevKey
    _ -> do
      parent <- Z.up z
      curI <- Z.currentIndex z
      let newI = case Z.branches parent of
            ObjectF hm -> do
              let keys = HM.keys hm
              (_, newKey) <- List.find ((curI ==) . Key . fst) (zip keys (drop 1 $ keys))
              pure $ Key newKey
            ArrayF xs -> case curI of
              (Index i) | i < (length xs - 1) -> pure . Index $ i + 1
              _ -> Nothing
            StringF {} -> Nothing
            NumberF {} -> Nothing
            BoolF {} -> Nothing
            NullF -> Nothing
      case newI of
        Just i -> Z.down i parent
        Nothing -> do
          next <- nextSibling <$> Z.up z
          fstI <- getFirstIdx (Z.branches next)
          Z.down fstI next

nextInList :: Eq a => a -> [a] -> Maybe a
nextInList a xs = fmap snd . List.find ((== a) . fst) $ zip xs (drop 1 xs)

prevInList :: Eq a => a -> [a] -> Maybe a
prevInList a xs = fmap snd . List.find ((== a) . fst) $ zip (drop 1 xs) xs

prevSibling :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
prevSibling z = fromMaybe z $ do
  case (Z.branches z, z ^. selectedKey_) of
    (ObjectF hm, Just k) -> do
      nextKey <- prevInList k $ HashMap.keys hm
      pure $ z & selectedKey_ ?~ nextKey
    _ -> do
      parent <- Z.up z
      curI <- Z.currentIndex z
      let newI = case Z.branches parent of
            ObjectF hm -> do
              let keys = HM.keys hm
              (newKey, _) <- List.find ((curI ==) . Key . snd) (zip keys (drop 1 $ keys))
              pure $ Key newKey
            ArrayF _ -> case curI of
              (Index 0) -> Nothing
              (Index i) -> pure . Index $ i - 1
              _ -> Nothing
            StringF {} -> Nothing
            NumberF {} -> Nothing
            BoolF {} -> Nothing
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
  ArrayF {} -> Just $ Index 0
  StringF {} -> Nothing
  NumberF {} -> Nothing
  BoolF {} -> Nothing
  NullF -> Nothing

getLastIdx :: ValueF x -> Maybe JIndex
getLastIdx = \case
  ObjectF hm -> Key . snd <$> unsnoc (HM.keys hm)
  ArrayF Empty -> Nothing
  ArrayF vec -> Just $ Index (length vec - 1)
  StringF {} -> Nothing
  NumberF {} -> Nothing
  BoolF {} -> Nothing
  NullF -> Nothing

newBuffer :: Text -> Buffer
newBuffer txt = TZ.gotoEOF $ TZ.textZipper (Text.lines txt) Nothing

into :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
into z =
  case (Z.branches z, z ^. selectedKey_) of
    (ObjectF _, Just key) -> do
      Z.tug (Z.down (Key key)) z
    (ObjectF hm, Nothing) -> do
      let fstKey = (HM.keys hm) ^? _head
      z & selectedKey_ .~ fstKey -- & buffer_ (Focused, Move) .~ newBuffer (fromMaybe "" fstKey)
    (ArrayF {}, _) ->
      fromMaybe z $ do
        Z.down (Index 0) z
    (StringF {}, _) -> z
    (NumberF {}, _) -> z
    (BoolF {}, _) -> z
    (NullF, _) -> z

outOf :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
outOf z =
  case (Z.branches z, z ^. selectedKey_) of
    (ObjectF _, Just k) -> do
      z & selectedKey_ .~ Nothing
        & Z.branches_ %~ \case
          ObjectF hm -> ObjectF $ renameKey k (z ^. bufferText_) hm
          _ -> error "expected object"
    _ -> Z.tug Z.up z

renderValue' :: RenderState -> Z.Zipper ValueF FocusState -> Image
renderValue' (foc, mode) z =
  Z.foldSpine alg $ (_rendered <$> rerenderFocus (foc, mode) z)
  where
    alg :: Image -> ValueF Image -> Image
    alg img = \case
      ObjectF hm -> prettyObj 2 Nothing (NotFocused, mode) hm
      ArrayF vec -> prettyArray 2 (NotFocused, mode) vec
      StringF {} -> img
      NumberF {} -> img
      BoolF {} -> img
      NullF -> img

rerenderFocus :: RenderState -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
rerenderFocus rs z = z & Z.focus_ . rendered .~ renderSubtree rs (z ^. Z.unwrapped_)

type RenderState = (Focused, ZMode)

data Focused = Focused | NotFocused
  deriving (Eq)

renderSubtree :: RenderState -> Cofree ValueF FocusState -> Vty.Image
renderSubtree rs@(foc, _) z = case (z ^. Cofree._unwrap, z ^. Cofree._extract . selectedKey) of
  (StringF _, _) -> indentLine i (colored green "\"" <|> bufImg green <|> colored green "\",")
  (NullF, _) -> indentLine i (colored Vty.yellow "null,")
  (NumberF _, _) -> indentLine i (bufImg blue <|> colored blue ",")
  (BoolF b, _) -> indentLine i (colored magenta (boolText_ # b) <|> Vty.char defAttr ',')
  (ArrayF xs, _) -> prettyArray i rs (renderChildren xs)
  (ObjectF xs, _) -> prettyObj i ((z ^. Cofree._extract . buffer,) <$> focusedKey) rs (renderChildren xs)
  where
    colored col txt =
      Vty.text' (Vty.defAttr `Vty.withForeColor` col `Vty.withStyle` if foc == Focused then Vty.reverseVideo else Vty.defaultStyleMask) txt
        <|> colorFix
    focusedKey :: Maybe Text
    focusedKey = z ^. Cofree._extract . selectedKey
    bufImg :: Vty.Color -> Image
    bufImg col = renderBuffer rs (Vty.defAttr `Vty.withForeColor` col) $ z ^. Cofree._extract . buffer
    renderChildren :: (Functor g, Functor f) => f (Cofree g FocusState) -> f Vty.Image
    renderChildren = fmap (_rendered . Comonad.extract)
    i = 2

renderBuffer :: RenderState -> Attr -> Buffer -> Image
renderBuffer (Focused, Move) attr buf = Vty.text' (Vty.withStyle attr Vty.reverseVideo) $ bufferText buf
renderBuffer (NotFocused, _) attr buf = Vty.text' attr $ bufferText buf
renderBuffer (Focused, _) attr buf =
  let (prefix, suffix) = Text.splitAt (snd $ TZ.cursorPosition buf) (bufferText buf)
      suffixImg = case Text.uncons suffix of
        Nothing -> Vty.char (Vty.withStyle attr Vty.reverseVideo) ' '
        Just (c, rest) -> Vty.char (Vty.withStyle attr Vty.reverseVideo) c <|> Vty.text' attr rest
   in Vty.text' attr prefix <|> suffixImg <|> colorFix

prettyArray :: Int -> RenderState -> Vector Image -> Vty.Image
prettyArray i rs vs =
  let inner :: [Image] = indented 2 (Vector.toList vs)
   in Vty.vertCat . indented i $ [img "["] ++ inner ++ [img "],"]
  where
    img t = case rs of
      (Focused, _) -> Vty.text' (Vty.withStyle Vty.defAttr Vty.reverseVideo) t <|> colorFix
      (NotFocused, _) -> Vty.text' Vty.defAttr t <|> colorFix

prettyObj :: Int -> Maybe (Buffer, Text) -> RenderState -> HashMap Text Vty.Image -> Vty.Image
prettyObj i focusedKey rs@(foc, _mode) vs =
  let inner :: [Image] =
        indented
          2
          ( HM.foldrWithKey'
              ( \k v a ->
                  [imgForKey k <|> Vty.text' defAttr ": ", v] ++ a
              )
              []
              vs
          )
   in Vty.vertCat . indented i $
        ([img "{"] ++ inner ++ [img "},"])
  where
    color = defAttr `withForeColor` cyan
    imgForKey k
      | Just (buf, fk) <- focusedKey,
        fk == k && foc == Focused =
        Vty.char color '"' <|> renderBuffer rs color buf <|> Vty.char color '"' <|> colorFix
      | otherwise = Vty.string color (show k)
    img t = case foc of
      Focused
        | Nothing <- focusedKey -> Vty.text' (Vty.withStyle Vty.defAttr Vty.reverseVideo) t <|> colorFix
      _ -> Vty.text' Vty.defAttr t <|> colorFix

colorFix :: Vty.Image
colorFix = Vty.text' defAttr ""

indented :: Functor f => Int -> f Vty.Image -> f Vty.Image
indented n xs = (indentLine n) <$> xs

indentLine :: Int -> Vty.Image -> Vty.Image
indentLine n x = (Vty.text' (Vty.withForeColor defAttr Vty.brightBlack) $ " " <> Text.replicate (n - 1) " ") <|> x

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
          img = renderSubtree (NotFocused, Move) (FS mempty buf Nothing :< rec)
          fs = FS img buf Nothing
       in fs :< rec

bufferFromValueF :: ValueF x -> Buffer
bufferFromValueF v = newBuffer $ v ^. valueFText
