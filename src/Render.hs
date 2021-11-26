module Render where

import Control.Monad.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prettyprinter

data Cursor = Cursor

renderScreen :: Int -> SimpleDocStream (Either Cursor ann) -> SimpleDocStream ann
renderScreen winHeight = flip evalState mempty . renderScreen' winHeight

renderScreen' :: Int -> SimpleDocStream (Either Cursor ann) -> State (Seq (SimpleDocStream (Either Cursor ann))) (SimpleDocStream ann)
renderScreen' n s = do
  modify (track half s)
  case s of
    SFail -> pure SFail
    SEmpty -> pure SEmpty
    SChar c sds -> SChar c <$> renderScreen' n sds
    SText i txt sds -> SText i txt <$> renderScreen' n sds
    SLine i sds -> SLine i <$> renderScreen' n sds
    SAnnPush (Left Cursor) sds -> do
      stash <- get
      case Seq.viewr stash of
        Seq.EmptyR -> pure $ takeN n (dropCursor sds)
        _ Seq.:> a -> pure $ takeN n (dropCursor a)
    SAnnPush (Right ann) sds -> SAnnPush ann <$> renderScreen' n sds
    SAnnPop sds -> SAnnPop <$> renderScreen' n sds
  where
    half = n `div` 2

dropCursor :: SimpleDocStream (Either Cursor ann) -> SimpleDocStream ann
dropCursor = \case
  SFail -> SFail
  SEmpty -> SEmpty
  SChar c sds -> SChar c $ dropCursor sds
  SText i txt sds -> SText i txt $ dropCursor sds
  SLine i sds -> SLine i $ dropCursor sds
  SAnnPush (Left Cursor) sds -> dropCursor sds
  SAnnPush (Right ann) sds -> SAnnPush ann $ dropCursor sds
  SAnnPop sds -> SAnnPop $ dropCursor sds

-- Tardis monad?
track :: Int -> a -> Seq a -> Seq a
track n a s
  | Seq.length s < n = a Seq.:<| s
  | otherwise = case Seq.viewr s of
    Seq.EmptyR -> Seq.singleton a
    -- Drop one element from the end
    rest Seq.:> _ -> a Seq.:<| rest

takeN :: Int -> SimpleDocStream ann -> SimpleDocStream ann
takeN 0 _ = SEmpty
takeN n s = do
  case s of
    SFail -> SFail
    SEmpty -> SEmpty
    SChar c sds -> SChar c $ takeN n sds
    SText i txt sds -> SText i txt $ takeN n sds
    SLine i sds -> SLine i $ takeN (n - 1) sds
    SAnnPush ann sds -> SAnnPush ann $ takeN n sds
    SAnnPop sds -> SAnnPop $ takeN n sds
