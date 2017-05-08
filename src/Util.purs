module Util where

import Prelude
import Node.FS.Aff
import Control.Monad.Aff (Aff, attempt, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, throw)
import Control.Monad.Except (throwError)
import Data.Array (init)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Node.Encoding (Encoding(..))
import Node.Path (FilePath)

foreign import sep :: String

parentPath :: FilePath -> Maybe FilePath
parentPath p = pure <<< (joinWith sep) =<< (init $ split (Pattern sep) p)

traverseFindDeep :: forall eff. Int -> FilePath -> String -> Aff (exception :: EXCEPTION, fs :: FS | eff ) (Either Error String)
traverseFindDeep 0    _   filename = liftEff $ throw $ "Could not find " <> filename <> " in the file tree.  Max search depth exceeded."
traverseFindDeep max path filename = do
  found <- attempt $ readTextFile UTF8 (path <> "/" <> filename)
  case found of
    Left e -> do
      realpath path >>= \p -> case (parentPath p) of
        Just parent -> traverseFindDeep (max - 1) parent filename
        Nothing -> liftEff $ throw $ "Could not find " <> filename <> " in the file tree."
    Right t -> pure $ Right t


traverseFind :: forall eff. FilePath -> String -> Aff (exception :: EXCEPTION, fs :: FS | eff ) (Either Error String)
traverseFind = traverseFindDeep 30
