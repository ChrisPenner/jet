{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import qualified Data.Text as Text
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Class

data PEnv = PEnv {indent :: Int, column :: Int, width :: Int, txt :: Text}
newtype Pretty a = Pretty (State PEnv a)
  deriving newtype (Functor, Applicative, Monad, MonadState PEnv)

remaining :: Pretty Int
remaining = gets (\PEnv{..} -> (width - column) - indent)

write :: Text -> Pretty ()
write t = do
  let lns = Text.lines t
  case lns of
    [] -> pure ()
    [h] -> do
      modify (\p -> p{txt=txt p <> t, column=column p + Text.length t})
      pure ()
    xs -> do
      i <- gets indent
      let total = Text.intercalate (Text.replicate i " ") xs
      let col = Text.length (last xs)
      modify (\p -> p{column=col + i})
      modify (\p -> p{txt=txt p <> total})


-- data PrettyNode a = PrettyNode
--   { size :: Int,
--     singleLine :: a,
--     multiLine :: [a]
--   }

-- data Pretty a =
--       N (PrettyNode a)
--     | Group [Pretty a]

-- atom :: Text -> Pretty Text
-- atom t = pure t

-- atom :: Text -> Pretty Text
-- atom t =
--   Pretty
--     { size = Text.length t,
--       singleLine = t,
--       multiLine = [t]
--     }



-- [
--     "hi",
--     {
--         "testing": [
--             1,
--             2,
--             3,
--             4
--         ]
--     },
--     [
--         4,
--         5,
--         6
--     ]
-- ]
