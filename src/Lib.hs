{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib (run) where

import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as Cofree
import qualified Control.Comonad.Trans.Cofree as CofreeF
import Control.Lens hiding ((:<))
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Extra
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Functor.Classes (Eq1 (..), Ord1 (liftCompare))
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
import qualified Render
import qualified System.Console.ANSI as ANSI
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Hclip
import Text.Read (readMaybe)
import qualified Zipper as Z

hoistMaybe :: Maybe a -> MaybeT Editor a
hoistMaybe = MaybeT . pure

data ZState = ZState
  { _undo :: [Z.Zipper ValueF FocusState],
    _mode :: ZMode,
    _register :: ValueF (Cofree ValueF FocusState)
  }

newtype Editor a = Editor {runEditor :: StateT ZState IO a}
  deriving newtype (Functor, Applicative, Monad, MonadState ZState, MonadIO)

mode_ :: Lens' ZState ZMode
mode_ = lens _mode (\s m -> s {_mode = m})

register_ :: Lens' ZState (ValueF (Cofree ValueF FocusState))
register_ = lens _register (\s m -> s {_register = m})

undo_ :: Lens' ZState [Z.Zipper ValueF FocusState]
undo_ = lens _undo (\s m -> s {_undo = m})

recover :: a -> MaybeT Editor a -> Editor a
recover def m = do
  let e = runMaybeT m
  s <- get
  r <- liftIO $ flip runStateT s . runEditor $ e
  case r of
    (Just a, newS) -> put newS *> pure a
    (Nothing, _) -> pure def

data Focused = Focused | NotFocused
  deriving (Eq)

type PrettyJSON = Doc (Either Render.Cursor AnsiStyle)

type Buffer = TZ.TextZipper Text

type FocusState = Focused

run :: IO ()
run = do
  json <-
    getArgs >>= \case
      -- [] -> Aeson.decode . BS.pack <$> getContents
      [f] -> Aeson.decodeFileStrict f
      _ -> putStrLn "usage: structural-json FILE.json" *> exitFailure
  result <- edit $ fromJust $ json
  BS.putStrLn $ encodePretty result

edit :: Value -> IO Value
edit value = do
  config <- liftIO $ Vty.standardIOConfig
  vty <- liftIO $ Vty.mkVty config
  let start = Z.zipper . toCofree $ value
  v <- flip evalStateT startState . runEditor $ loop vty start
  liftIO $ Vty.shutdown vty
  pure (Z.flatten v)

loop ::
  Vty.Vty ->
  Z.Zipper ValueF FocusState ->
  Editor (Z.Zipper ValueF FocusState)
loop vty z = do
  winHeight <- liftIO . fmap Vty.regionHeight . Vty.displayBounds . Vty.outputIface $ vty
  rendered <- uses mode_ (\m -> fullRender m z)
  let screen = renderStrict . Render.renderScreen winHeight . layoutSmart defaultLayoutOptions $ rendered
  renderScreen screen
  e <- liftIO $ Vty.nextEvent vty
  nextZ <- handleEvent z e
  if (maybeQuit e)
    then pure nextZ
    else (loop vty nextZ)
  where

-- Force the screen to whnf before we clear the old screen.
-- Otherwise it does too much work after clearing but before rendering.
renderScreen :: Text -> Editor ()
renderScreen !screen = do
  liftIO $ ANSI.clearScreen
  liftIO $ ANSI.setCursorPosition 0 0
  liftIO $ Text.putStr screen

pushUndo :: Z.Zipper ValueF FocusState -> Editor ()
pushUndo z =
  undo_ %= \case
    undos@(head' : _) | head' == z -> undos
    undos -> z : undos

-- logMode :: Editor ()
-- logMode = do
--   mode <- use mode_
--   liftIO $ appendFile "log.txt" (show mode <> "\n")

startState :: ZState
startState =
  ZState
    { _undo = [],
      _mode = Move,
      _register = NullF
    }

maybeQuit :: Vty.Event -> Bool
maybeQuit = \case
  EvKey (KChar 'c') [Vty.MCtrl] -> True
  EvKey (KChar 'q') [] -> True
  _ -> False

bufferText :: Buffer -> Text
bufferText = Text.concat . TZ.getText

applyBuf :: Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
applyBuf z = do
  use mode_ >>= \case
    Edit buf -> do
      let txt = buf ^. to bufferText
      mode_ .= Move
      pure
        ( z & Z.unwrapped_ . _unwrap
            %~ ( \case
                   StringF _ -> StringF txt
                   (NumberF n) -> NumberF . fromMaybe n . readMaybe $ Text.unpack txt
                   x -> x
               )
        )
    KeyEdit key buf -> do
      let txt = buf ^. to bufferText
      mode_ .= (KeyMove txt)
      pure
        ( z & Z.unwrapped_ . _unwrap
            %~ ( \case
                   (ObjectF hm) -> ObjectF $ renameKey key txt hm
                   x -> x
               )
        )
    _ -> pure z

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

handleEvent :: Z.Zipper ValueF FocusState -> Vty.Event -> Editor (Z.Zipper ValueF FocusState)
handleEvent z evt = do
  use mode_ >>= \case
    KeyMove {} -> handleMove
    Move {} -> handleMove
    KeyEdit {} -> handleEdit
    Edit {} -> handleEdit
  where
    handleEdit =
      case evt of
        EvKey key [] ->
          case key of
            KChar c -> do
              mode_ . buf_ %= TZ.insertChar c
              pure z
            KLeft -> do
              mode_ . buf_ %= TZ.moveLeft
              pure z
            KRight -> do
              mode_ . buf_ %= TZ.moveRight
              pure z
            KBS -> do
              mode_ . buf_ %= TZ.deletePrevChar
              pure z
            KEsc -> do
              applyBuf z
            _ -> pure z
        _ -> pure z
    handleMove =
      case evt of
        EvKey key _mods -> case key of
          KChar 'h' -> z & outOf
          KChar 'l' -> z & into
          KChar 'j' -> z & sibling Forward
          KChar 'J' -> do
            pushUndo z
            pure (z & moveElement Forward)
          KChar 'K' -> do
            pushUndo z
            pure (z & moveElement Backward)
          KChar 'k' -> z & sibling Backward
          KChar 'i' -> do
            pushUndo z
            use mode_ >>= \case
              KeyMove k -> do
                mode_ .= KeyEdit k (newBuffer k)
                pure z
              Move
                | Just editBuf <- bufferFrom z -> do
                  mode_ .= Edit editBuf
                  pure z
              _ -> pure z
          KChar 'b' -> do
            pushUndo z
            pure (z & setFocus (BoolF True))
          KChar 'o' -> do
            pushUndo z
            pure (z & setFocus (ObjectF mempty))
          KChar 'a' -> do
            pushUndo z
            pure (z & setFocus (ArrayF mempty))
          KChar 'n' -> do
            pushUndo z
            pure (z & setFocus (NumberF 0))
          KChar 's' -> do
            pushUndo z
            pure (z & setFocus (StringF ""))
          KChar 'u' -> do
            -- TODO: replace undo state with a zipper to add redo
            use undo_ >>= \case
              (lastZ : rest) -> do
                undo_ .= rest
                pure lastZ
              _ -> pure z
          KChar ' ' -> do
            pushUndo z
            pure (z & tryToggle)
          KChar 'y' -> do
            let curVal = Z.branches z
            register_ .= curVal
            liftIO $ setClipboard (encodeValueFCofree curVal)
            pure z
          KChar 'p' -> do
            reg <- use register_
            pure (z & setFocus reg)
          KEnter -> do
            pushUndo z
            tryInsert z
          KBS -> do
            pushUndo z
            delete z
          _ -> pure z
        _ -> pure z

encodeValueFCofree :: ValueF (Cofree ValueF FocusState) -> String
encodeValueFCofree vf = LBS.unpack . encodePretty . FF.embed $ fmap (FF.cata alg) vf
  where
    alg :: CofreeF.CofreeF ValueF Focused Value -> Value
    alg (_ CofreeF.:< vf') = FF.embed vf'

setFocus :: ValueF (Cofree ValueF FocusState) -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
setFocus f z = z & Z.branches_ .~ f

data Dir = Forward | Backward

moveElement :: Dir -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
moveElement dir z = fromMaybe z $ do
  i <- case Z.currentIndex z of
    Just (Index i) -> pure i
    _ -> Nothing
  parent <- Z.up z
  pure $
    case parent ^. Z.branches_ of
      ArrayF arr ->
        let swapI = case dir of
              Forward -> i + 1
              Backward -> i - 1
            moves =
              [ (i, arr Vector.!? swapI),
                (swapI, arr Vector.!? i)
              ]
                & sequenceOf (traversed . _2)
                & fromMaybe []
         in parent
              & Z.branches_ .~ ArrayF (arr Vector.// moves)
              & fromMaybe z . Z.down (Index swapI)
      _ -> z

tryToggle :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
tryToggle z =
  z & Z.branches_ %~ \case
    BoolF b -> BoolF (not b)
    x -> x

tryInsert :: Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
tryInsert z =
  z & Z.branches_ %%~ \case
    ObjectF hm -> do
      mode_ .= (KeyEdit "" $ newBuffer "")
      pure $ ObjectF $ HM.insert "" (NotFocused :< NullF) hm
    ArrayF arr -> do
      mode_ .= Move
      pure $ ArrayF $ arr <> pure (NotFocused :< NullF)
    x -> pure x

delete :: Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
delete z = do
  curMode <- use mode_
  mode_ .= Move
  pure $ case z ^. Z.branches_ of
    -- If we're in a Key focus, delete that key
    ObjectF hm
      | KeyMove k <- curMode -> (z & Z.branches_ .~ ObjectF (HM.delete k hm))
    -- Otherwise move up a layer and delete the key we were in.
    _ -> case Z.currentIndex z of
      -- If we don't have a parent, set the current node to null
      Nothing -> z & Z.branches_ .~ NullF
      Just i -> fromMaybe z $ do
        parent <- Z.up z
        pure $
          parent & Z.branches_ %~ \case
            ObjectF hm | Key k <- i -> ObjectF (HM.delete k hm)
            ArrayF arr | Index j <- i -> ArrayF (Vector.ifilter (\i' _ -> i' /= j) arr)
            x -> x

sibling :: Dir -> Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
sibling dir z = recover z $ do
  mode <- use mode_
  case (mode, Z.branches z) of
    (KeyMove k, ObjectF hm) -> do
      case findSiblingIndex (== k) $ HashMap.keys hm of
        Nothing -> pure z
        Just theKey -> do
          mode_ .= KeyMove theKey
          pure z
    _ -> do
      parent <- hoistMaybe $ Z.up z
      curI <- hoistMaybe $ Z.currentIndex z
      let newI = case Z.branches parent of
            ObjectF hm -> do
              let keys = HM.keys hm
              newKey <- findSiblingIndex (\k -> Key k == curI) keys
              pure $ Key newKey
            ArrayF xs -> case curI of
              (Index i) -> alterIndex xs i
              _ -> Nothing
            StringF {} -> Nothing
            NumberF {} -> Nothing
            BoolF {} -> Nothing
            NullF -> Nothing
      case newI of
        Just i -> hoistMaybe $ Z.down i parent
        Nothing -> hoistMaybe Nothing
  where
    -- nextZ <- lift $ sibling dir parent
    -- fstI <- hoistMaybe $ getFirstIdx (Z.branches nextZ)
    -- hoistMaybe $ Z.down fstI nextZ

    (findSiblingIndex, alterIndex) = case dir of
      Forward ->
        ( findAfter,
          \xs i -> if i < length xs - 1 then Just (Index (i + 1)) else Nothing
        )
      Backward ->
        ( findBefore,
          \_xs i -> if i > 0 then Just (Index (i -1)) else Nothing
        )

findAfter :: (a -> Bool) -> [a] -> Maybe a
findAfter p xs = fmap snd . List.find (p . fst) $ zip xs (drop 1 xs)

findBefore :: (a -> Bool) -> [a] -> Maybe a
findBefore p xs = fmap snd . List.find (p . fst) $ zip (drop 1 xs) xs

-- getFirstIdx :: ValueF x -> Maybe JIndex
-- getFirstIdx = \case
--   ObjectF hm -> Key . fst <$> uncons (HM.keys hm)
--   ArrayF Empty -> Nothing
--   ArrayF {} -> Just $ Index 0
--   StringF {} -> Nothing
--   NumberF {} -> Nothing
--   BoolF {} -> Nothing
--   NullF -> Nothing

-- getLastIdx :: ValueF x -> Maybe JIndex
-- getLastIdx = \case
--   ObjectF hm -> Key . snd <$> unsnoc (HM.keys hm)
--   ArrayF Empty -> Nothing
--   ArrayF vec -> Just $ Index (length vec - 1)
--   StringF {} -> Nothing
--   NumberF {} -> Nothing
--   BoolF {} -> Nothing
--   NullF -> Nothing

newBuffer :: Text -> Buffer
newBuffer txt = TZ.gotoEOF $ TZ.textZipper (Text.lines txt) Nothing

into :: Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
into z = do
  mode <- use mode_
  case (Z.branches z, mode) of
    (ObjectF _, KeyMove key) -> do
      mode_ .= Move
      pure (Z.tug (Z.down (Key key)) z)
    (ObjectF hm, Move) -> do
      case (HM.keys hm) ^? _head of
        Just fstKey -> do
          mode_ .= KeyMove fstKey
          pure z
        _ -> pure z
    (ArrayF {}, _) -> do
      pure $ Z.tug (Z.down (Index 0)) z
    _ -> pure z

outOf :: Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
outOf z = do
  mode <- use mode_
  case (Z.branches z, mode) of
    (ObjectF _, KeyMove {}) -> do
      mode_ .= Move
      pure z
    _ -> pure (Z.tug Z.up z)

-- Render the given zipper
fullRender :: ZMode -> Z.Zipper ValueF FocusState -> PrettyJSON
fullRender mode z = do
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
  (StringF txt) -> cursor foc $ case (foc, mode) of
    (Focused, Edit buf) ->
      indent i (colored' Green "\"" <> renderBuffer Green buf <> colored' Green "\",")
    _ -> indent i (colored' Green "\"" <> colored' Green (Text.unpack txt) <> colored' Green "\",")
  (NullF) -> cursor foc $ indent i (colored' Yellow "null,")
  (NumberF n) -> cursor foc $ case (foc, mode) of
    (Focused, Edit buf) -> indent i (renderBuffer Blue buf <> colored' Blue ",")
    _ -> indent i (colored' Blue (show n) <> colored' Blue ",")
  (BoolF b) -> cursor foc $ indent i (colored' Magenta (Text.unpack $ boolText_ # b) <> pretty ',')
  (ArrayF xs) -> prettyArray foc xs
  (ObjectF xs) -> prettyObj foc mode xs
  where
    colored' :: Color -> String -> PrettyJSON
    colored' col txt =
      P.annotate (Right $ if foc == Focused then reverseCol col else colorDull col) (pretty txt)
    i = 2

reverseCol :: Color -> AnsiStyle
reverseCol = \case
  White -> colorDull Black <> bgColorDull White
  Green -> colorDull Black <> bgColorDull Green
  c -> bgColorDull c

prettyWith :: Pretty a => AnsiStyle -> a -> PrettyJSON
prettyWith ann a = annotate (Right ann) $ pretty a

colored :: Pretty a => Color -> a -> PrettyJSON
colored col a = annotate (Right $ colorDull col) $ pretty a

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
   in cursor foc $ vsep $ [img "[", indent 2 (vsep inner), img "],"]
  where
    img :: Text -> PrettyJSON
    img t = case foc of
      Focused -> prettyWith (reverseCol White) t
      NotFocused -> pretty t

cursor :: Focused -> PrettyJSON -> PrettyJSON
cursor Focused = P.annotate (Left Render.Cursor)
cursor _ = id

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
      rendered = vsep [img "{", indent 2 inner, img "},"]
   in case mode of
        Move -> cursor focused rendered
        _ -> rendered
  where
    imgForKey k = case focused of
      NotFocused -> colored Cyan (show k)
      Focused -> case mode of
        KeyMove focKey | focKey == k -> cursor Focused $ prettyWith (reverseCol Cyan) (show focKey)
        KeyEdit focKey buf | focKey == k -> cursor Focused $ colored Cyan '"' <> renderBuffer Cyan buf <> colored Cyan '"'
        _ -> colored Cyan (show k)
    img :: Text -> PrettyJSON
    img t = case (focused, mode) of
      (Focused, Move) -> prettyWith (reverseCol White) t
      _ -> pretty t

instance Eq1 ValueF where
  liftEq f vf1 vf2 = case (vf1, vf2) of
    (ObjectF l, ObjectF r) -> liftEq f l r
    (ArrayF l, ArrayF r) -> liftEq f l r
    (NullF, NullF) -> True
    (StringF l, StringF r) -> l == r
    (NumberF l, NumberF r) -> l == r
    (BoolF l, BoolF r) -> l == r
    _ -> False

instance Ord1 ValueF where
  liftCompare f vf1 vf2 = case (vf1, vf2) of
    (ObjectF l, ObjectF r) -> liftCompare f l r
    (ArrayF l, ArrayF r) -> liftCompare f l r
    (NullF, NullF) -> EQ
    (StringF l, StringF r) -> compare l r
    (NumberF l, NumberF r) -> compare l r
    (BoolF l, BoolF r) -> compare l r
    (NullF, _) -> LT
    (_, NullF) -> GT
    (BoolF _, _) -> LT
    (_, BoolF _) -> GT
    (NumberF _, _) -> LT
    (_, NumberF _) -> GT
    (StringF _, _) -> LT
    (_, StringF _) -> GT
    (ArrayF _, _) -> LT
    (_, ArrayF _) -> GT

data JIndex
  = Index Int
  | Key Text
  deriving (Show, Eq, Ord)

instance FunctorWithIndex JIndex ValueF

instance FoldableWithIndex JIndex ValueF

instance TraversableWithIndex JIndex ValueF where
  itraverse f = \case
    NullF -> pure NullF
    StringF txt -> pure (StringF txt)
    NumberF sci -> pure (NumberF sci)
    BoolF b -> pure (BoolF b)
    ObjectF hm -> ObjectF <$> itraverse (\k a -> f (Key k) a) hm
    ArrayF arr -> ArrayF <$> itraverse (\k a -> f (Index k) a) arr

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
