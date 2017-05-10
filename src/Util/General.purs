module Util.General where

import Prelude
import Node.FS.Aff
import Control.Monad.Aff (Aff, attempt, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, throw)
import Control.Monad.Except (throwError)
import Data.Array (foldM, init)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Node.Encoding (Encoding(..))
import Node.Path (FilePath)

foreign import sep :: String

parentPath :: FilePath -> Maybe FilePath
parentPath p = pure <<< (joinWith sep) =<< (init $ split (Pattern sep) p)

traverseFindDeep :: forall eff. Int -> FilePath -> String -> Aff (exception :: EXCEPTION, fs :: FS | eff ) String -- (Either Error String)
traverseFindDeep 0    _   filename = liftEff $ throw $ "Could not find " <> filename <> " in the file tree.  Max search depth exceeded."
traverseFindDeep max path filename = do
  found <- attempt $ readTextFile UTF8 (path <> "/" <> filename)
  case found of
    Left e -> do
      realpath path >>= \p -> case (parentPath p) of
        Just parent -> traverseFindDeep (max - 1) parent filename
        Nothing -> liftEff $ throw $ "Could not find " <> filename <> " in the file tree."
    Right t -> pure t


traverseFind :: forall eff. FilePath -> String -> Aff (exception :: EXCEPTION, fs :: FS | eff ) String -- (Either Error String)
traverseFind = traverseFindDeep 30

forM :: forall a m. (Monad m) => Array a -> (a -> m Unit) -> m Unit
forM v fn = foldM (\a v' -> fn v') unit v

whenTrue :: forall a. Boolean -> a -> a -> a
whenTrue isTrue tFn fFn =
  if isTrue
    then tFn
    else fFn

meither :: forall a b. Maybe a -> b -> Either b a
meither a b = case a of
  Just aa -> Right aa
  Nothing -> Left b

maybeFail :: forall a b. String -> Maybe a -> Either String a
maybeFail s m = case m of
  Just mm -> Right mm
  Nothing -> Left s

failWith :: forall f c. (Bifunctor f) => String -> f String c -> f String c
failWith s = lmap (\a -> s)

failAppend :: forall f c. (Bifunctor f) => String -> f String c -> f String c
failAppend s = lmap (_ <> s)

failPrepend :: forall f c. (Bifunctor f) => String -> f String c -> f String c
failPrepend s = lmap (s <> _)

-- glob :: forall e. String -> Aff (fs :: FS | e) Array FilePath
-- glob s = runFn1globImpl s
--
