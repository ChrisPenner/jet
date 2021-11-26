module Render where

import Control.Lens
import Control.Monad.State
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as Text
import qualified Graphics.Vty as Vty
import Prettyprinter
import Prettyprinter.Render.Util.StackMachine (renderSimplyDecoratedA)

data Cursor = Cursor

-- renderScreen :: Int -> SimpleDocStream (Either Cursor ann) -> SimpleDocStream ann
-- renderScreen winHeight = flip evalState mempty . renderScreen' winHeight

-- renderScreen' :: Int -> SimpleDocStream (Either Cursor ann) -> State (Seq (SimpleDocStream (Either Cursor ann))) (SimpleDocStream ann)
-- renderScreen' n s = do
--   modify (track half s)
--   case s of
--     SFail -> pure SFail
--     SEmpty -> pure SEmpty
--     SChar c sds -> SChar c <$> renderScreen' n sds
--     SText i txt sds -> SText i txt <$> renderScreen' n sds
--     SLine i sds -> SLine i <$> renderScreen' n sds
--     SAnnPush (Left Cursor) sds -> do
--       stash <- get
--       case Seq.viewr stash of
--         Seq.EmptyR -> pure $ takeN n (dropCursor sds)
--         _ Seq.:> a -> pure $ takeN n (dropCursor a)
--     SAnnPush (Right ann) sds -> SAnnPush ann <$> renderScreen' n sds
--     SAnnPop sds -> SAnnPop <$> renderScreen' n sds
--   where
--     half = n `div` 2

-- dropCursor :: SimpleDocStream (Either Cursor ann) -> SimpleDocStream ann
-- dropCursor = \case
--   SFail -> SFail
--   SEmpty -> SEmpty
--   SChar c sds -> SChar c $ dropCursor sds
--   SText i txt sds -> SText i txt $ dropCursor sds
--   SLine i sds -> SLine i $ dropCursor sds
--   SAnnPush (Left Cursor) sds -> dropCursor sds
--   SAnnPush (Right ann) sds -> SAnnPush ann $ dropCursor sds
--   SAnnPop sds -> SAnnPop $ dropCursor sds

-- -- Tardis monad?
-- track :: Int -> a -> Seq a -> Seq a
-- track n a s
--   | Seq.length s < n = a Seq.:<| s
--   | otherwise = case Seq.viewr s of
--     Seq.EmptyR -> Seq.singleton a
--     -- Drop one element from the end
--     rest Seq.:> _ -> a Seq.:<| rest

-- takeN :: Int -> SimpleDocStream ann -> SimpleDocStream ann
-- takeN 0 _ = SEmpty
-- takeN n s = do
--   case s of
--     SFail -> SFail
--     SEmpty -> SEmpty
--     SChar c sds -> SChar c $ takeN n sds
--     SText i txt sds -> SText i txt $ takeN n sds
--     SLine i sds -> SLine i $ takeN (n - 1) sds
--     SAnnPush ann sds -> SAnnPush ann $ takeN n sds
--     SAnnPop sds -> SAnnPop $ takeN n sds

data Output = LineBreak | ImgChunk Vty.Image

renderScreen :: Int -> SimpleDocStream (Either Cursor Vty.Attr) -> [Vty.Image]
renderScreen winHeight doc = do
  let (outputs, (_, _, mcursor)) =
        doc
          & toLineStream
          & flip runState ([], 0, Nothing)

  let allLines =
        outputs & List.foldr collapse ([], [])
          & ( \(buf, rest) ->
                if null buf
                  then rest
                  else Vty.horizCat buf : rest
            )

  let cropped = case mcursor of
        Nothing -> allLines
        Just cursorPos -> takeSurroundingCursor winHeight cursorPos allLines
  cropped
  where
    collapse out (buf, rest) =
      case out of
        LineBreak -> ([], Vty.horizCat buf : rest)
        ImgChunk img -> (img : buf, rest)

takeSurroundingCursor :: Int -> Int -> [a] -> [a]
takeSurroundingCursor height cursorPos xs
  | cursorPos - half > 0 =
    xs
      & drop (cursorPos - half)
      & take (half + 1)
  | otherwise = take height xs
  where
    half = height `div` 2

toLineStream :: SimpleDocStream (Either Cursor Vty.Attr) -> State ([Vty.Attr], Int, Maybe Int) [Output]
toLineStream doc =
  renderSimplyDecoratedA
    renderText
    pushAnn
    popAnn
    doc
  where
    popAnn _ = do
      _1 %= drop 1
      pure mempty
    pushAnn = \case
      Left Cursor -> do
        cursorLine <- use _2
        _3 ?= cursorLine
        pure []
      Right ann -> do
        _1 %= (ann :)
        pure mempty
    renderText txt = do
      attr <- uses _1 (fromMaybe Vty.defAttr . listToMaybe)
      let newLines = List.intersperse LineBreak $ (ImgChunk . Vty.text' attr) <$> Text.lines txt
          totalNewLines = max (length newLines - 1) 0
      _2 += totalNewLines
      pure newLines
