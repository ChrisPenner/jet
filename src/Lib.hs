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
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.Text.IO as Text
import Data.Text.Zipper as TZ
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as Vty
import Text.Read (readMaybe)
import qualified Zipper as Z
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Prettyprinter as P
import Prettyprinter.Render.Terminal as P
import qualified System.Console.ANSI as ANSI

type PrettyJSON = Doc AnsiStyle

type Buffer = TZ.TextZipper Text

data FocusState = FS
  { _rendered :: PrettyJSON,
    _buffer :: Buffer,
    _selectedKey :: Maybe Text
  }

makeLenses ''FocusState

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
  config <- liftIO $ Vty.standardIOConfig
  vty <- liftIO $ Vty.mkVty config
  let start = Z.zipper . toCofree $ value
  let loop mode prevZ z = do
        let !screen = renderStrict $ layoutSmart defaultLayoutOptions (renderValue' (Focused, mode) z)
        ANSI.clearScreen
        ANSI.setCursorPosition 0 0
        liftIO $ Text.putStr screen
        e <- liftIO $ Vty.nextEvent vty
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

renderValue' :: RenderState -> Z.Zipper ValueF FocusState -> PrettyJSON
renderValue' (foc, mode) z =
  Z.foldSpine alg $ (_rendered <$> rerenderFocus (foc, mode) z)
  where
    alg :: PrettyJSON -> ValueF PrettyJSON -> PrettyJSON
    alg img = \case
      ObjectF hm -> prettyObj Nothing (NotFocused, mode) hm
      ArrayF vec -> prettyArray (NotFocused, mode) vec
      StringF {} -> img
      NumberF {} -> img
      BoolF {} -> img
      NullF -> img

rerenderFocus :: RenderState -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
rerenderFocus rs z = z & Z.focus_ . rendered .~ renderSubtree rs (z ^. Z.unwrapped_)

type RenderState = (Focused, ZMode)

data Focused = Focused | NotFocused
  deriving (Eq)

renderSubtree :: RenderState -> Cofree ValueF FocusState -> PrettyJSON
renderSubtree rs@(foc, _) z = case (z ^. Cofree._unwrap, z ^. Cofree._extract . selectedKey) of
  (StringF _, _) -> indent i (colored' Green "\"" <> bufImg Green <> colored' Green "\",")
  (NullF, _) -> indent i (colored' Yellow "null,")
  (NumberF _, _) -> indent i (bufImg Blue <> colored' Blue ",")
  (BoolF b, _) -> indent i (colored' Magenta (boolText_ # b) <> pretty ',')
  (ArrayF xs, _) -> prettyArray rs (renderChildren xs)
  (ObjectF xs, _) -> prettyObj ((z ^. Cofree._extract . buffer,) <$> focusedKey) rs (renderChildren xs)
  where
    colored' :: Color -> Text -> PrettyJSON
    colored' col txt =
      P.annotate (if foc == Focused then reverseCol col else colorDull col) (pretty txt)
    focusedKey :: Maybe Text
    focusedKey = z ^. Cofree._extract . selectedKey
    bufImg :: Color -> PrettyJSON
    bufImg col = renderBuffer rs col $ z ^. Cofree._extract . buffer
    renderChildren :: (Functor g, Functor f) => f (Cofree g FocusState) -> f PrettyJSON
    renderChildren = fmap (_rendered . Comonad.extract)
    i = 2

reverseCol :: Color -> AnsiStyle
reverseCol = \case
  White -> colorDull Black <> bgColorDull White
  Green -> colorDull Black <> bgColorDull Green
  c -> bgColorDull c

prettyWith :: Pretty a => ann -> a -> Doc ann
prettyWith ann a = annotate ann $ pretty a

colored :: Pretty a => Color -> a -> Doc AnsiStyle
colored col a = annotate (colorDull col) $ pretty a

renderBuffer :: RenderState -> Color -> Buffer -> PrettyJSON
renderBuffer (Focused, Move) col buf = prettyWith (reverseCol col) $ bufferText buf
renderBuffer (NotFocused, _) col buf = colored col $ bufferText buf
renderBuffer (Focused, _) col buf =
  let (prefix, suffix) = Text.splitAt (snd $ TZ.cursorPosition buf) (bufferText buf)
      suffixImg = case Text.uncons suffix of
        Nothing -> prettyWith (reverseCol col) ' '
        Just (c, rest) -> prettyWith (reverseCol col) c <> colored col rest
   in colored col prefix <> suffixImg

prettyArray :: RenderState -> Vector PrettyJSON -> PrettyJSON
prettyArray rs vs =
  let inner :: [PrettyJSON] = (Vector.toList vs)
   in vsep $ [img "[", indent 2 (vsep inner), img "],"]
  where
    img :: Text -> PrettyJSON
    img t = case rs of
      (Focused, _) -> prettyWith (reverseCol White) t
      (NotFocused, _) -> pretty t

prettyObj :: Maybe (Buffer, Text) -> RenderState -> HashMap Text PrettyJSON -> PrettyJSON
prettyObj focusedKey rs@(foc, _mode) vs =
  let inner :: PrettyJSON
      inner = vsep
          ( HM.toList vs <&>
              ( \(k, v) ->
                  vsep [imgForKey k <> pretty @Text ": ", indent 2 v]
              )
          )
   in vsep [img "{", indent 2 inner, img "},"]
  where
    imgForKey k
      | Just (buf, fk) <- focusedKey,
        fk == k && foc == Focused =
        colored Cyan '"' <> renderBuffer rs Cyan buf <> colored Cyan '"'
      | otherwise = colored Cyan (show k)
    img :: Text -> PrettyJSON
    img t = case foc of
      Focused
        | Nothing <- focusedKey -> prettyWith (reverseCol White) t
      _ -> pretty t

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
