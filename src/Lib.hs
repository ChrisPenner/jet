{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
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

import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as Cofree
import Control.Lens hiding ((:<))
import Control.Monad.State
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Extra
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
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events
import Prettyprinter as P
import Prettyprinter.Render.Terminal as P
import qualified System.Console.ANSI as ANSI
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import qualified Zipper as Z

data Focused = Focused | NotFocused
  deriving (Eq)

type PrettyJSON = Doc AnsiStyle

type Buffer = TZ.TextZipper Text

type FocusState = Focused

run :: IO ()
run = do
  json <-
    getArgs >>= \case
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
        let !screen = renderStrict $ layoutSmart defaultLayoutOptions (fullRender mode z)
        ANSI.clearScreen
        ANSI.setCursorPosition 0 0
        liftIO $ Text.putStr screen
        e <- liftIO $ Vty.nextEvent vty
        let (nextMode, nextZ) = handleEvent mode prevZ z e
        appendFile "log.txt" (show nextMode <> "\n")
        -- let nextZ = rerenderFocus nextMode updatedZ
        if (maybeQuit e)
          then pure nextZ
          else (loop nextMode z nextZ)
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

applyBuf :: ZMode -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
applyBuf mode z =
  case mode of
    Edit buf ->
      let txt = buf ^. to bufferText
       in z & Z.unwrapped_ . _unwrap
            %~ ( \case
                   StringF _ -> StringF txt
                   (NumberF n) -> NumberF . fromMaybe n . readMaybe $ Text.unpack txt
                   x -> x
               )
    KeyEdit key buf -> do
      let txt = buf ^. to bufferText
       in z & Z.unwrapped_ . _unwrap
            %~ ( \case
                   (ObjectF hm) -> ObjectF $ renameKey key txt hm
                   x -> x
               )
    Move -> z
    KeyMove {} -> z

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

data ZMode
  = Edit {_buf :: Buffer}
  | Move
  | KeyMove {_selectedKey :: Text}
  | KeyEdit {_selectedKey :: Text, _buf :: Buffer}
  deriving (Show)

buf_ :: Traversal' ZMode Buffer
buf_ f = \case
  Edit b -> Edit <$> f b
  Move -> pure Move
  KeyMove txt -> pure (KeyMove txt)
  KeyEdit txt b -> KeyEdit txt <$> f b

handleEvent :: ZMode -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState -> Vty.Event -> (ZMode, Z.Zipper ValueF FocusState)
handleEvent mode prevZ z evt =
  case mode of
    KeyMove {} -> handleMove
    Move {} -> handleMove
    KeyEdit {} -> handleEdit
    Edit {} -> handleEdit
  where
    handleEdit =
      case evt of
        EvKey key [] ->
          case key of
            KChar c -> (mode & buf_ %~ TZ.insertChar c, z)
            KLeft -> (mode & buf_ %~ TZ.moveLeft, z)
            KRight -> (mode & buf_ %~ TZ.moveRight, z)
            KBS -> (mode & buf_ %~ TZ.deletePrevChar, z)
            KEsc -> do
              (Move, z & applyBuf mode)
            _ -> (mode, z)
        _ -> (mode, z)
    handleMove =
      case evt of
        EvKey key _mods -> case key of
          KChar 'h' -> z & outOf mode
          KChar 'l' -> z & into mode
          KChar 'j' -> z & nextSibling mode
          KChar 'k' -> z & prevSibling mode
          KChar 'i'
            | Just editBuf <- bufferFrom z -> (Edit editBuf, z)
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
setFocus f z = z & Z.branches_ .~ f

tryToggle :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
tryToggle z =
  z & Z.branches_ %~ \case
    BoolF b -> BoolF (not b)
    x -> x

nextSibling :: ZMode -> Z.Zipper ValueF FocusState -> (ZMode, Z.Zipper ValueF FocusState)
nextSibling mode z = fromMaybe (mode, z) $ do
  case (mode, Z.branches z) of
    (KeyMove k, ObjectF hm) -> do
      prevKey <- findAfter (== k) $ HashMap.keys hm
      pure $ (KeyMove prevKey, z)
    _ -> do
      parent <- Z.up z
      curI <- Z.currentIndex z
      let newI = case Z.branches parent of
            ObjectF hm -> do
              let keys = HM.keys hm
              newKey <- findAfter (\k -> Key k == curI) keys
              pure $ Key newKey
            ArrayF xs -> case curI of
              (Index i) | i < (length xs - 1) -> pure . Index $ i + 1
              _ -> Nothing
            StringF {} -> Nothing
            NumberF {} -> Nothing
            BoolF {} -> Nothing
            NullF -> Nothing
      case newI of
        Just i -> (mode,) <$> Z.down i parent
        Nothing -> do
          (nextMode, nextZ) <- nextSibling mode <$> Z.up z
          fstI <- getFirstIdx (Z.branches nextZ)
          (nextMode,) <$> Z.down fstI nextZ

findAfter :: (a -> Bool) -> [a] -> Maybe a
findAfter p xs = fmap snd . List.find (p . fst) $ zip xs (drop 1 xs)

findBefore :: (a -> Bool) -> [a] -> Maybe a
findBefore p xs = fmap snd . List.find (p . fst) $ zip (drop 1 xs) xs

prevSibling :: ZMode -> Z.Zipper ValueF FocusState -> (ZMode, Z.Zipper ValueF FocusState)
prevSibling mode z = fromMaybe (mode, z) $ do
  case (mode, Z.branches z) of
    (KeyMove k, ObjectF hm) -> do
      prevKey <- findBefore (== k) $ HashMap.keys hm
      pure $ (KeyMove prevKey, z)
    _ -> do
      parent <- Z.up z
      curI <- Z.currentIndex z
      let newI = case Z.branches parent of
            ObjectF hm -> do
              let keys = HM.keys hm
              newKey <- findBefore (\k -> Key k == curI) keys
              pure $ Key newKey
            ArrayF xs -> case curI of
              (Index i) | i < (length xs - 1) -> pure . Index $ i + 1
              _ -> Nothing
            StringF {} -> Nothing
            NumberF {} -> Nothing
            BoolF {} -> Nothing
            NullF -> Nothing
      case newI of
        Just i -> (mode,) <$> Z.down i parent
        Nothing -> do
          (prevMode, prevZ) <- prevSibling mode <$> Z.up z
          lstI <- getLastIdx (Z.branches prevZ)
          (prevMode,) <$> Z.down lstI prevZ

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

into :: ZMode -> Z.Zipper ValueF FocusState -> (ZMode, Z.Zipper ValueF FocusState)
into mode z =
  case (Z.branches z, mode) of
    (ObjectF _, KeyMove key) -> do
      (Move, Z.tug (Z.down (Key key)) z)
    (ObjectF hm, Move) -> do
      case (HM.keys hm) ^? _head of
        Just fstKey -> (KeyMove fstKey, z)
        _ -> (mode, z)
    (ArrayF {}, _) ->
      (mode, Z.tug (Z.down (Index 0)) z)
    _ -> (mode, z)

outOf :: ZMode -> Z.Zipper ValueF FocusState -> (ZMode, Z.Zipper ValueF FocusState)
outOf mode z =
  case (Z.branches z, mode) of
    (ObjectF _, KeyMove {}) -> (Move, z)
    _ -> (mode, Z.tug Z.up z)

-- Render the given zipper
fullRender :: ZMode -> Z.Zipper ValueF FocusState -> PrettyJSON
fullRender mode z =
  Z.fold alg $ (z & Z.focus_ .~ Focused)
  where
    alg foc vf = renderSubtree foc mode vf

-- where
--   alg :: PrettyJSON -> ValueF PrettyJSON -> PrettyJSON
--   alg img = \case
--     ObjectF hm -> prettyObj NotFocused mode hm
--     ArrayF vec -> prettyArray NotFocused vec
--     StringF {} -> img
--     NumberF {} -> img
--     BoolF {} -> img
--     NullF -> img

-- rerenderFocus :: ZMode -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
-- rerenderFocus mode z = z & Z.focus_ . rendered .~ renderSubtree Focused mode (z ^. Z.unwrapped_)

-- | Renders a subtree
renderSubtree :: Focused -> ZMode -> ValueF PrettyJSON -> PrettyJSON
renderSubtree foc mode vf = case vf of
  (StringF txt) -> case (foc, mode) of
    (Focused, Edit buf) ->
      indent i (colored' Green "\"" <> renderBuffer Green buf <> colored' Green "\",")
    _ -> indent i (colored' Green "\"" <> colored' Green (Text.unpack txt) <> colored' Green "\",")
  (NullF) -> indent i (colored' Yellow "null,")
  (NumberF n) -> case (foc, mode) of
    (Focused, Edit buf) -> indent i (renderBuffer Blue buf <> colored' Blue ",")
    _ -> indent i (colored' Blue (show n) <> colored' Blue ",")
  (BoolF b) -> indent i (colored' Magenta (Text.unpack $ boolText_ # b) <> pretty ',')
  (ArrayF xs) -> prettyArray foc xs
  (ObjectF xs) -> prettyObj foc mode xs
  where
    colored' :: Color -> String -> PrettyJSON
    colored' col txt =
      P.annotate (if foc == Focused then reverseCol col else colorDull col) (pretty txt)
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

renderBuffer :: Color -> Buffer -> PrettyJSON
renderBuffer col buf =
  let (prefix, suffix) = Text.splitAt (snd $ TZ.cursorPosition buf) (bufferText buf)
      suffixImg = case Text.uncons suffix of
        Nothing -> prettyWith (reverseCol col) ' '
        Just (c, rest) -> prettyWith (reverseCol col) c <> colored col rest
   in colored col prefix <> suffixImg

prettyArray :: Focused -> Vector PrettyJSON -> PrettyJSON
prettyArray foc vs =
  let inner :: [PrettyJSON] = (Vector.toList vs)
   in vsep $ [img "[", indent 2 (vsep inner), img "],"]
  where
    img :: Text -> PrettyJSON
    img t = case foc of
      Focused -> prettyWith (reverseCol White) t
      NotFocused -> pretty t

prettyObj :: Focused -> ZMode -> HashMap Text PrettyJSON -> PrettyJSON
prettyObj focused mode vs =
  let inner :: PrettyJSON
      inner =
        vsep
          ( HM.toList vs
              <&> ( \(k, v) ->
                      vsep [imgForKey k <> pretty @Text ": ", indent 2 v]
                  )
          )
   in vsep [img "{", indent 2 inner, img "},"]
  where
    imgForKey k = case focused of
      NotFocused -> colored Cyan (show k)
      Focused -> case mode of
        KeyMove focKey | focKey == k -> prettyWith (reverseCol Cyan) (show focKey)
        KeyEdit focKey buf | focKey == k -> colored Cyan '"' <> renderBuffer Cyan buf <> colored Cyan '"'
        _ -> colored Cyan (show k)
    img :: Text -> PrettyJSON
    img t = case (focused, mode) of
      (Focused, Move) -> prettyWith (reverseCol White) t
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
toCofree t = Cofree.unfold (\b -> (NotFocused, FF.project b)) t

-- where
--   go x =
--     let rec = fmap toCofree (FF.project x)
--         buf = bufferFromValueF $ FF.project x
--         img = renderSubtree NotFocused Move rec
--         fs = FS img buf Nothing
--      in fs :< rec

-- bufferFromValueF :: ValueF x -> Buffer
-- bufferFromValueF v = newBuffer $ v ^. valueFText

bufferFrom :: Z.Zipper ValueF a -> Maybe Buffer
bufferFrom z = newBuffer <$> (Z.branches z ^? valueFText)
