{-# LANGUAGE OverloadedStrings #-}

module Render where

import Control.Lens
import Control.Monad.State
import qualified Data.List as List
import Data.Maybe
import qualified Graphics.Vty as Vty
import Prettyprinter
import Prettyprinter.Render.Util.StackMachine (renderSimplyDecoratedA)

data Cursor = Cursor
  deriving (Show, Eq)

data Output = LineBreak | ImgChunk Vty.Image

renderScreen :: Int -> SimpleDocStream (Either Cursor Vty.Attr) -> [Vty.Image]
renderScreen winHeight doc = do
  let (outputs, (_, _, mcursor)) =
        doc
          & toLineStream
          & flip runState ([], 0, Nothing)

  let allLines =
        outputs & List.foldr collapse ([], [])
          -- clean up anything remaining in the buffer
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

-- Take only enough lines to fill the screen, with the cursor centered don't print any more.
takeSurroundingCursor :: Int -> Int -> [a] -> [a]
takeSurroundingCursor height cursorPos xs
  | cursorPos - half > 0 =
    xs
      & drop (cursorPos - half)
      & take height
  | otherwise = take height xs
  where
    half = height `div` 2

toLineStream ::
  SimpleDocStream (Either Cursor Vty.Attr) ->
  State ([Vty.Attr], Int, Maybe Int) [Output]
toLineStream doc =
  renderSimplyDecoratedA
    renderText
    pushAnn
    popAnn
    doc
  where
    popAnn _ = do
      _1 %= drop 1
      -- Need to clear existing colors or they bleed to the right.
      pure [ImgChunk (Vty.text' Vty.defAttr "")]
    pushAnn = \case
      Left Cursor -> do
        cursorLine <- use _2
        _3 ?= cursorLine
        pure []
      Right ann -> do
        _1 %= (ann :)
        pure mempty
    -- prettyprinter always renders lines as a single text fragment
    renderText "\n" = do
      _2 += 1
      pure [LineBreak]
    -- prettyprinter never passes text with newlines here
    renderText txt = do
      attr <- uses _1 (fromMaybe Vty.defAttr . listToMaybe)
      pure [ImgChunk (Vty.text' attr txt)]
