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

import Control.Category ((>>>))
import Control.Comonad (extract)
import Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as CofreeF
import Control.Exception (bracket)
import Control.Lens hiding ((:<))
import qualified Control.Lens.Cons as Cons
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
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Zipper as TZ
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events
import Prettyprinter as P
import qualified Render
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Hclip
import System.IO (IOMode (ReadWriteMode), openFile)
import qualified System.IO as IO
import qualified System.Posix as Posix
import Text.Read (readMaybe)
import qualified Zipper as Z

tabSize :: Int
tabSize = 2

maxUndoStates :: Int
maxUndoStates = 100

hoistMaybe :: Maybe a -> MaybeT Editor a
hoistMaybe = MaybeT . pure

data ZState = ZState
  { _undo :: UndoZipper (Z.Zipper ValueF FocusState),
    _mode :: ZMode,
    _register :: ValueF (Cofree ValueF FocusState),
    _vty :: Vty.Vty,
    _flash :: Text,
    _save :: Z.Zipper ValueF FocusState -> Editor ()
  }

newtype Editor a = Editor {runEditor :: StateT ZState IO a}
  deriving newtype (Functor, Applicative, Monad, MonadState ZState, MonadIO)

mode_ :: Lens' ZState ZMode
mode_ = lens _mode (\s m -> s {_mode = m})

register_ :: Lens' ZState (ValueF (Cofree ValueF FocusState))
register_ = lens _register (\s m -> s {_register = m})

undo_ :: Lens' ZState (UndoZipper (Z.Zipper ValueF FocusState))
undo_ = lens _undo (\s m -> s {_undo = m})

vty_ :: Lens' ZState Vty.Vty
vty_ = lens _vty (\s m -> s {_vty = m})

flash_ :: Lens' ZState Text
flash_ = lens _flash (\s m -> s {_flash = m})

save_ :: Lens' ZState (Z.Zipper ValueF FocusState -> Editor ())
save_ = lens _save (\s m -> s {_save = m})

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

data Folded = Folded | NotFolded
  deriving (Eq)

type PrettyJSON = Doc (Either Render.Cursor Vty.Attr)

type Buffer = TZ.TextZipper Text

data FocusState = FocusState
  { isFocused :: Focused,
    isFolded :: Folded,
    rendered :: PrettyJSON
  }

instance Eq FocusState where
  a == b =
    isFocused a == isFocused b
      && isFolded a == isFolded b

focused_ :: Lens' FocusState Focused
focused_ = lens isFocused (\fs new -> fs {isFocused = new})

folded_ :: Lens' FocusState Folded
folded_ = lens isFolded (\fs new -> fs {isFolded = new})

toggleFold :: Folded -> Folded
toggleFold Folded = NotFolded
toggleFold NotFolded = Folded

run :: IO ()
run = do
  (json, srcFile) <-
    getArgs >>= \case
      [] -> do
        json <-
          (Aeson.eitherDecode . BS.pack <$> getContents) >>= \case
            Left err -> do
              IO.hPutStrLn IO.stderr err
              exitFailure
            Right json -> pure json
        pure (json, Nothing)
      [f] -> do
        json <-
          Aeson.eitherDecodeFileStrict f >>= \case
            Left err -> do
              IO.hPutStrLn IO.stderr err
              exitFailure
            Right json -> pure json
        pure (json, Just f)
      _ -> putStrLn "usage: structural-json FILE.json" *> exitFailure
  result <- edit srcFile $ json
  BS.putStrLn $ encodePretty result

edit :: Maybe FilePath -> Value -> IO Value
edit srcFile value = do
  tty <- openFile "/dev/tty" ReadWriteMode >>= Posix.handleToFd
  config <- liftIO $ Vty.standardIOConfig
  vty <- (liftIO $ Vty.mkVty config {Vty.inputFd = Just tty, Vty.outputFd = Just tty})
  let start = Z.zipper . toCofree $ value
  v <- flip evalStateT (startState srcFile vty) . runEditor $ loop start
  Vty.shutdown vty
  pure (Z.flatten v)

loop ::
  Z.Zipper ValueF FocusState ->
  Editor (Z.Zipper ValueF FocusState)
loop z = do
  (winWidth, winHeight) <- bounds
  vty <- use vty_
  rendered <- uses mode_ (\m -> fullRender m z)
  footer <- footerImg
  let screen = Vty.vertCat . Render.renderScreen (winHeight - Vty.imageHeight footer) . layoutPretty defaultLayoutOptions $ rendered
  let spacerHeight = winHeight - (Vty.imageHeight screen + Vty.imageHeight footer)
  let spacers = Vty.charFill Vty.defAttr ' ' winWidth spacerHeight
  liftIO $ Vty.update vty (Vty.picForImage (screen Vty.<-> spacers Vty.<-> footer))
  flash_ .= ""
  e <- liftIO $ Vty.nextEvent vty
  nextZ <- handleEvent e z
  if (maybeQuit e)
    then pure nextZ
    else (loop nextZ)

bounds :: Editor (Int, Int)
bounds = use vty_ >>= liftIO . Vty.displayBounds . Vty.outputIface

footerImg :: Editor Vty.Image
footerImg = do
  (w, _) <- bounds
  flash <- gets _flash
  let attr = (Vty.defAttr `Vty.withForeColor` Vty.green `Vty.withStyle` Vty.reverseVideo)
      helpMsg = Vty.text' attr "| Press '?' for help"
      flashMsg = Vty.text' (attr `Vty.withStyle` Vty.bold) (" " <> flash)
  pure $
    Vty.horizCat
      [ flashMsg,
        Vty.charFill attr ' ' (w - (Vty.imageWidth helpMsg + Vty.imageWidth flashMsg)) 1,
        helpMsg
      ]

pushUndo :: Z.Zipper ValueF FocusState -> Editor ()
pushUndo z =
  undo_ %= \case
    (UndoZipper (ls Cons.:> _) _) | length ls >= maxUndoStates -> UndoZipper (z <| ls) Empty
    (UndoZipper ls _) -> UndoZipper (z <| ls) Empty

-- log :: Show a => a -> Editor ()
-- log a = do
--   liftIO $ appendFile "log.txt" (show a <> "\n")

startState :: Maybe FilePath -> Vty.Vty -> ZState
startState srcFile vty =
  ZState
    { _undo = UndoZipper Empty Empty,
      _mode = Move,
      _register = NullF,
      _vty = vty,
      _flash = "Hello World",
      _save = saveFile
    }
  where
    saveFile = case srcFile of
      Nothing -> const (pure ())
      Just fp -> \z -> do
        liftIO $ BS.writeFile fp (z & Z.flatten & encodePretty @Value)
        flash_ .= "Saved to " <> Text.pack fp

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

handleEvent :: Vty.Event -> Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
handleEvent evt zipper = do
  use mode_ >>= \case
    KeyMove {} -> handleMove zipper
    Move {} -> handleMove zipper
    KeyEdit {} -> handleEdit zipper
    Edit {} -> handleEdit zipper
  where
    handleEdit z =
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
              newZ <- applyBuf z
              pure $ newZ
            _ -> pure z
        _ -> pure z
    handleMove z =
      case evt of
        EvKey key mods -> case key of
          KChar 'h' -> z & outOf
          KChar 'l' -> do
            z & Z.focus_ . folded_ .~ NotFolded
              & into
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
            insert z
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
          KChar 'N' -> do
            pushUndo z
            pure (z & setFocus NullF)
          KChar 's'
            | [Vty.MCtrl] <- mods -> do
              saver <- use save_
              saver z
              pure z
          KChar 's' -> do
            pushUndo z
            pure (z & setFocus (StringF ""))
          KChar 'u' -> do
            flash_ .= "Undo"
            undo_ %%= \case
              (UndoZipper (l Cons.:< ls) rs) ->
                (l, UndoZipper ls (z Cons.:< rs))
              lz -> (z, lz)
          KChar 'r' | [Vty.MCtrl] <- mods -> do
            flash_ .= "Redo"
            undo_ %%= \case
              (UndoZipper ls (r Cons.:< rs)) -> (r, UndoZipper (z Cons.:< ls) rs)
              lz -> (z, lz)
          KChar ' ' -> do
            pushUndo z
            pure (z & tryToggle)
          KChar 'y' -> do
            flash_ .= "Copied"
            copy z
          KChar 'p' -> do
            flash_ .= "Paste"
            pushUndo z
            paste z
          KChar 'x' -> do
            flash_ .= "Cut"
            pushUndo z
            copy z >>= delete
          KChar '?' -> do
            vty <- use vty_
            liftIO $ Vty.update vty (Vty.picForImage helpImg)
            void $ liftIO $ Vty.nextEvent vty
            pure z
          KEnter -> do
            pushUndo z
            tryAddChild z
          KChar '\t' -> do
            -- Exit KeyMove mode if we're in it.
            mode_ .= Move
            pure $ (z & Z.focus_ . folded_ %~ toggleFold)
          KChar 'F' -> do
            -- Fold all child branches
            pure $ mapChildren (mapped . folded_ .~ Folded) z
          KChar 'f' -> do
            -- Unfold all child branches
            pure $ mapChildren (mapped . folded_ .~ NotFolded) z
          KBS -> do
            flash_ .= "Deleted"
            pushUndo z
            delete z
          _ -> pure z
        _ -> pure z
    paste z = do
      reg <- use register_
      pure (z & setFocus reg)
    copy z = do
      let curVal = Z.branches z
      register_ .= curVal
      liftIO $ setClipboard (encodeValueFCofree curVal)
      pure z
    insert z = do
      use mode_ >>= \case
        KeyMove k -> do
          mode_ .= KeyEdit k (newBuffer k)
          pure $ z & Z.focus_ . folded_ .~ NotFolded
        Move
          | Just editBuf <- bufferFrom z -> do
            mode_ .= Edit editBuf
            pure $ z & Z.focus_ . folded_ .~ NotFolded
        _ -> pure z

encodeValueFCofree :: ValueF (Cofree ValueF FocusState) -> String
encodeValueFCofree vf = LBS.unpack . encodePretty . FF.embed $ fmap (FF.cata alg) vf
  where
    alg :: CofreeF.CofreeF ValueF ann Value -> Value
    alg (_ CofreeF.:< vf') = FF.embed vf'

setFocus :: ValueF (Cofree ValueF FocusState) -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
setFocus f z = z & Z.branches_ .~ f & rerender

data Dir = Forward | Backward

moveElement :: Dir -> Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
moveElement dir z = fromMaybe z $ do
  i <- case Z.currentIndex z of
    Just (Index i) -> pure i
    _ -> Nothing
  parent <- z & rerender & Z.up
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

tryAddChild :: Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
tryAddChild z =
  z & Z.branches_ %%~ \case
    ObjectF hm -> do
      mode_ .= (KeyEdit "" $ newBuffer "")
      pure $ ObjectF $ HM.insert "" (toCofree Aeson.Null) hm
    ArrayF arr -> do
      mode_ .= Move
      pure $ ArrayF $ arr <> pure (toCofree Aeson.Null)
    x -> pure x

delete :: Z.Zipper ValueF FocusState -> Editor (Z.Zipper ValueF FocusState)
delete z = do
  curMode <- use mode_
  mode_ .= Move
  pure $ case z ^. Z.branches_ of
    -- If we're in a Key focus, delete that key
    ObjectF hm
      | KeyMove k <- curMode ->
        ( z & Z.branches_ .~ ObjectF (HM.delete k hm)
        )
    -- Otherwise move up a layer and delete the key we were in.
    _ -> case Z.currentIndex z of
      -- If we don't have a parent, set the current node to null
      Nothing ->
        z & Z.branches_ .~ NullF
      Just i -> fromMaybe z $ do
        parent <- z & rerender & Z.up
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
      curI <- hoistMaybe $ Z.currentIndex z
      parent <- hoistMaybe $ (z & rerender & Z.up)
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
  maybeParentKey <- case (Z.currentIndex z) of
    Just (Key k) -> pure $ Just k
    _ -> pure Nothing

  case (Z.branches z, mode) of
    (ObjectF _, KeyMove {}) -> do
      mode_ .= Move
      pure z
    _ -> do
      maybe (pure ()) (\k -> mode_ .= KeyMove k) maybeParentKey
      pure (Z.tug (rerender >>> Z.up) z)

-- Render the given zipper using render caches stored in each node.
fullRender :: ZMode -> Z.Zipper ValueF FocusState -> PrettyJSON
fullRender mode z = do
  let focusedRender =
        z & Z.focus_ . focused_ .~ Focused
          & Z.unwrapped_ %~ \(fs :< vf) ->
            let rerendered = renderSubtree fs mode (rendered . extract <$> vf)
             in (fs {rendered = rerendered} :< vf)
  rendered . Z.foldSpine alg $ focusedRender
  where
    alg fs vf =
      fs {rendered = rerenderCached fs (rendered <$> vf)}
    rerenderCached fs = \case
      ObjectF o -> prettyObj (isFocused fs) mode o
      ArrayF a -> prettyArray (isFocused fs) a
      -- Nodes without children are never part of the spine, but just to have something
      -- we can render the cache.
      _ -> rendered fs

-- | Updates the cached render of the current focus, using cached renders for subtrees.
rerender :: Z.Zipper ValueF FocusState -> Z.Zipper ValueF FocusState
rerender = Z.unwrapped_ %~ rerenderCofree

-- Rerenders a layer of a cofree structure. Doesn't re-render the children.
rerenderCofree :: Cofree ValueF FocusState -> Cofree ValueF FocusState
rerenderCofree (fs :< vf) =
  let rerendered = (renderSubtree fs mode (rendered . extract <$> vf))
   in fs {rendered = rerendered} :< vf
  where
    -- Currently the mode is required by renderSubtree, but for the rerender cache it's
    -- irrelevant, because it only matters if we're 'focused', and if we're focused, we'll be
    -- manually rerendered later anyways.
    mode = Move

-- | Renders a subtree
renderSubtree :: FocusState -> ZMode -> ValueF PrettyJSON -> PrettyJSON
renderSubtree (FocusState {isFolded = Folded, isFocused}) _ vf = case vf of
  ObjectF {} -> colored' Vty.white "{...}"
  ArrayF {} -> colored' Vty.white "[...]"
  StringF {} -> colored' Vty.green "\"...\""
  NumberF {} -> colored' Vty.blue "..."
  NullF {} -> colored' Vty.yellow "..."
  BoolF {} -> colored' Vty.magenta "..."
  where
    colored' :: Vty.Color -> String -> PrettyJSON
    colored' col txt =
      P.annotate (Right $ if isFocused == Focused then reverseCol col else Vty.defAttr `Vty.withForeColor` col) (pretty txt)
renderSubtree (FocusState {isFocused}) mode vf = case vf of
  (StringF txt) -> cursor isFocused $ case (isFocused, mode) of
    (Focused, Edit buf) ->
      colored' Vty.green "\"" <> renderBuffer Vty.green buf <> colored' Vty.green "\""
    _ -> colored' Vty.green "\"" <> colored' Vty.green (Text.unpack txt) <> colored' Vty.green "\""
  (NullF) -> cursor isFocused $ colored' Vty.yellow "null"
  (NumberF n) -> cursor isFocused $ case (isFocused, mode) of
    (Focused, Edit buf) -> renderBuffer Vty.blue buf
    _ -> colored' Vty.blue (show n)
  (BoolF b) -> cursor isFocused $ colored' Vty.magenta (Text.unpack $ boolText_ # b)
  (ArrayF xs) -> prettyArray isFocused xs
  (ObjectF xs) -> prettyObj isFocused mode xs
  where
    colored' :: Vty.Color -> String -> PrettyJSON
    colored' col txt =
      P.annotate (Right $ if isFocused == Focused then reverseCol col else Vty.defAttr `Vty.withForeColor` col) (pretty txt)

reverseCol :: Vty.Color -> Vty.Attr
reverseCol col = Vty.defAttr `Vty.withForeColor` col `Vty.withStyle` Vty.reverseVideo

mapChildren ::
  (Cofree ValueF FocusState -> Cofree ValueF FocusState) ->
  Z.Zipper ValueF FocusState ->
  Z.Zipper ValueF FocusState
mapChildren f = Z.branches_ . mapped %~ FF.cata alg
  where
    alg :: CofreeF.CofreeF ValueF FocusState (Cofree ValueF FocusState) -> Cofree ValueF FocusState
    alg (cf CofreeF.:< vf) = rerenderCofree $ f (cf :< vf)

prettyWith :: Pretty a => Vty.Attr -> a -> PrettyJSON
prettyWith ann a = annotate (Right ann) $ pretty a

colored :: Pretty a => Vty.Color -> a -> PrettyJSON
colored col a = annotate (Right $ Vty.defAttr `Vty.withForeColor` col) $ pretty a

renderBuffer :: Vty.Color -> Buffer -> PrettyJSON
renderBuffer col buf =
  let (prefix, suffix) = Text.splitAt (snd $ TZ.cursorPosition buf) (bufferText buf)
      suffixImg = case Text.uncons suffix of
        Nothing -> prettyWith (reverseCol col) ' '
        Just (c, rest) -> prettyWith (reverseCol col) c <> colored col rest
   in colored col prefix <> suffixImg

prettyArray :: Focused -> Vector PrettyJSON -> PrettyJSON
prettyArray foc vs =
  let inner :: [PrettyJSON] =
        Vector.toList vs
          & imap (\i v -> v <> commaKey i)
   in cursor foc $ vsep $ [img "[", indent tabSize (vsep inner), img "]"]
  where
    img :: Text -> PrettyJSON
    img t = case foc of
      Focused -> prettyWith (reverseCol Vty.white) t
      NotFocused -> pretty t
    commaKey i
      | i == Vector.length vs - 1 = mempty
      | otherwise = ","

cursor :: Focused -> PrettyJSON -> PrettyJSON
cursor Focused = P.annotate (Left Render.Cursor)
cursor _ = id

prettyObj :: Focused -> ZMode -> HashMap Text PrettyJSON -> PrettyJSON
prettyObj focused mode vs =
  let inner :: PrettyJSON
      inner =
        vsep
          ( HM.toList vs
              & imap
                ( \i (k, v) ->
                    vsep [imgForKey k <> pretty @Text ": ", indent tabSize (v <> commaKey i)]
                )
          )
      rendered = vsep [img "{", indent tabSize inner, img "}"]
   in case mode of
        Move -> cursor focused rendered
        _ -> rendered
  where
    hmSize = HM.size vs
    commaKey i
      | i == hmSize - 1 = mempty
      | otherwise = ","
    imgForKey k = case focused of
      NotFocused -> colored Vty.cyan (show k)
      Focused -> case mode of
        KeyMove focKey | focKey == k -> cursor Focused $ prettyWith (reverseCol Vty.cyan) (show focKey)
        KeyEdit focKey buf | focKey == k -> cursor Focused $ colored Vty.cyan '"' <> renderBuffer Vty.cyan buf <> colored Vty.cyan '"'
        _ -> colored Vty.cyan (show k)
    img :: Text -> PrettyJSON
    img t = case (focused, mode) of
      (Focused, Move) -> prettyWith (reverseCol Vty.white) t
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
toCofree t = FF.hylo alg FF.project $ t
  where
    defaultFs = FocusState NotFocused NotFolded mempty
    mode = Move
    alg :: ValueF (Cofree ValueF FocusState) -> Cofree ValueF FocusState
    alg vf = defaultFs {rendered = renderSubtree defaultFs mode (rendered . extract <$> vf)} :< vf

bufferFrom :: Z.Zipper ValueF a -> Maybe Buffer
bufferFrom z = newBuffer <$> (Z.branches z ^? valueFText)

helpImg :: Vty.Image
helpImg =
  let helps =
        [ ("h", "ascend"),
          ("l", "descend"),
          ("j", "next sibling"),
          ("k", "previous sibling"),
          ("J", "move down (in array)"),
          ("K", "move up (in array)"),
          ("i", "enter edit mode (string/number)"),
          ("<C-s>", "save file"),
          ("<SPACE>", "toggle boolean"),
          ("<ESC>", "exit edit mode"),
          ("<BS>", "delete key/element"),
          ("<ENTER>", "add new key/element (object/array)"),
          ("<TAB>", "toggle fold"),
          ("f", "unfold all children"),
          ("F", "fold all children"),
          ("s", "replace element with string"),
          ("b", "replace element with bool"),
          ("n", "replace element with number"),
          ("N", "replace element with null"),
          ("a", "replace element with array"),
          ("o", "replace element with object"),
          ("u", "undo last change (undo buffer keeps 100 states)"),
          ("<C-r>", "redo from undo states"),
          ("y", "copy current value into buffer (and clipboard)"),
          ("p", "paste value from buffer over current value"),
          ("x", "cut a value, equivalent to a copy -> delete"),
          ("q | ctrl-c", "quit without saving. Due to a bug, tap twice")
        ]

      (keys, descs) =
        unzip
          ( helps <&> \(key, desc) ->
              ( Vty.text' (Vty.defAttr `Vty.withForeColor` Vty.green) (key <> ": "),
                Vty.text' Vty.defAttr desc
              )
          )
   in (Vty.vertCat keys Vty.<|> Vty.vertCat descs)

data UndoZipper a
  = UndoZipper
      (Seq a)
      -- ^ undo states
      (Seq a)
      -- ^ redo states
