{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import qualified Control.Comonad as Comonad
import Control.Comonad.Cofree
import qualified Control.Comonad.Cofree as Cofree
import Control.Lens
import Control.Monad
import Data.Aeson (Value (String))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as P
import Data.Aeson.Extra
import qualified Data.ByteString.Lazy as BS
import qualified Data.Functor.Foldable as FF
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
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
import qualified Data.List as List

run :: IO ()
run = do
  void $ edit $ fromJust $ Aeson.decode ("[\"hi\", {\"testing\":[1, 2, 3, 4]}, [4, 5, 6]]")

edit :: Value -> IO Value
edit value = do
  config <- standardIOConfig
  vty <- mkVty config
  let start = fromJust $ (Z.down (ArrI 0)) . Z.zipper . renderedZipper . toCofree $ value
  let loop i foc = do
        update vty . Vty.picForImage $ renderValue' foc
        e <- nextEvent vty
        let (i', foc') = handleEvent foc i e
        if (maybeQuit e)
          then pure foc'
          else (loop i' foc')
  v <- loop (head $ Z.ixes $ Z.branches start) start
  Vty.shutdown vty
  pure (Z.flatten v)

maybeQuit :: Vty.Event -> Bool
maybeQuit = \case
  EvKey (KChar 'c') [Vty.MCtrl] -> True
  _ -> False

defIndex :: Z.Idx f => Z.Zipper f a -> Z.IxOf f
defIndex = head . Z.ixes . Z.branches

withDefIndex :: Z.Idx f => Z.Zipper f a -> (Z.IxOf f, Z.Zipper f a)
withDefIndex z = (defIndex z, z)

handleEvent :: Z.Zipper ValueF a -> JIndex -> Vty.Event ->  (JIndex, Z.Zipper ValueF a)
handleEvent z i = \case
  EvKey key mods -> case key of
    KChar 'h' -> withDefIndex $ Z.tug Z.up z
    KChar 'l' -> withDefIndex $ Z.tug (Z.down i) z
    KChar 'j' ->
      let indexes = Z.ixes (Z.branches z)
          currentI = List.findIndex (== i) $ indexes
          nextI = currentI >>= \i -> indexes ^? ix i
       in withDefIndex $ Z.tug (\z' -> join (Z.sibling <$> nextI <*> pure z')) z
    _ -> (i, z)
  -- KChar c -> _
  -- KEsc -> _
  -- KBS -> _
  -- KEnter -> _
  -- KLeft -> _
  -- KRight -> _
  -- KUp -> _
  -- KDown -> _
  -- KUpLeft -> _
  -- KUpRight -> _
  -- KDownLeft -> _
  -- KDownRight -> _
  -- KCenter -> _
  -- KFun n -> _
  -- KBackTab -> _
  -- KPrtScr -> _
  -- KPause -> _
  -- KIns -> _
  -- KHome -> _
  -- KPageUp -> _
  -- KDel -> _
  -- KEnd -> _
  -- KPageDown -> _
  -- KBegin -> _
  -- KMenu -> _
  _ -> (i, z)

-- renderValue :: Z.Zipper ValueF a -> Text
-- renderValue z = Text.decodeUtf8 . BS.toStrict . P.encodePretty @Value $ Z.flatten z

renderValue' :: Z.Zipper ValueF Vty.Image -> Vty.Image
renderValue' z =
  let newA = toImage 0 (Vty.withStyle defAttr Vty.reverseVideo) (z ^. Z.unwrapped . _unwrap . to (fmap Comonad.extract))
   in Z.foldSpine (toImage 0 defAttr) $ (z & Z.focus .~ newA)

toImage :: Int -> Attr -> ValueF Image -> Image
toImage i attr (StringF t) = indentLine i (Vty.text' attr ("\"" <> t <> "\""))
toImage i attr NullF = indentLine i (Vty.text' attr "null")
toImage i attr (NumberF n) = indentLine i (Vty.text' attr (Text.pack $ show n))
toImage i attr (BoolF b) = indentLine i (Vty.text' attr (Text.pack $ show b))
toImage i attr (ArrayF xs) = prettyArray i attr xs
toImage i attr (ObjectF xs) = prettyObj i attr xs

renderedZipper :: Cofree ValueF x -> Cofree ValueF Image
renderedZipper = Z.retag (toImage 0 defAttr)

atom :: Text -> Vty.Image
atom t = Vty.text' defAttr t

prettyArray :: Int -> Attr -> Vector Vty.Image -> Vty.Image
prettyArray i attr vs =
  let inner :: [Image] = indented 1 (Vector.toList vs)
   in Vty.vertCat . indented i $ [Vty.char attr '['] ++ inner ++ [Vty.char attr ']']

prettyObj :: Int -> Attr -> HashMap Text Vty.Image -> Vty.Image
prettyObj i attr vs =
  let inner :: [Image] = indented 1 (HM.foldrWithKey' (\k v a -> [Vty.string (Vty.withForeColor defAttr Vty.red) (show k) <|> Vty.text' defAttr ": ", v] ++ a) [] vs)
   in Vty.vertCat . indented i $
        ([Vty.char attr '{'] ++ inner ++ [Vty.char attr '}'])

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
  = ArrI Int
  | ObjI Text
  | NullI
  deriving (Show, Eq, Ord)

instance Z.Idx ValueF where
  type IxOf ValueF = JIndex
  idx :: Z.IxOf ValueF -> Traversal' (ValueF a) a
  idx (ArrI i) f (ArrayF xs) = ArrayF <$> ix i f xs
  idx (ObjI k) f (ObjectF xs) = ObjectF <$> ix k f xs
  idx _ _ x = pure x

  ixes = \case
    ObjectF hm -> ObjI <$> HM.keys hm
    ArrayF vec -> ArrI <$> [0..Vector.length vec - 1]
    StringF txt -> [NullI]
    NumberF sci -> [NullI]
    BoolF b -> [NullI]
    NullF -> [NullI]

toCofree :: (Value -> Cofree ValueF ())
toCofree t = Cofree.unfold (\x -> ((), FF.project x)) t
