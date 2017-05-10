module Main where

import Prelude
import Assets
import Control.Monad.Aff.Console as Affc
import Data.Array.Partial as PartialArray
import Data.Map as M
import Data.StrMap as StrMap
import Node.FS as FS
import Node.FS.Async as Async
import Control.Alt ((<|>))
import Control.Alternative (empty, when)
import Control.Lazy (defer)
import Control.Monad.Aff (Aff, attempt, launchAff, makeAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Data.Array (all, filter, find, foldM, foldMap, length, reverse, snoc, some, head)
import Data.Either (Either(..), either)
import Data.Eq (class Eq, eq)
import Data.Foldable (sum)
import Data.Foreign (F, Foreign, readArray, readString, toForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.List.Lazy (List, fromFoldable, singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid)
import Data.Monoid.Conj (Conj(..))
import Data.Ord (greaterThan)
import Data.Semigroup (class Semigroup)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.YAML.Foreign.Decode (parseYAML)
import Data.YAML.Foreign.Encode (class ToYAML, printYAML, toYAML, valueToYAML, YObject, YValue(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (stat, FS)
import Node.FS.Stream (createWriteStream)
import Node.FS.Sync (exists)
import Node.Process (PROCESS, exit)
import Node.Stream (uncork, writeString)
import Node.Yargs (Yargs)
import Node.Yargs.Applicative (Y, flag, rest, runY, yarg)
import Node.Yargs.Setup (YargsSetup, defaultHelp, defaultVersion, example, string, usage, version)
import Partial.Unsafe (unsafePartial)
import Util.General (forM)

data Which = All | One String | Many (Array String)

exampleConfig :: String
exampleConfig = """
- name: development
  paths:
    - development.html
- name: production
  paths:
    - production.html
    - credentials/auth.json
"""

app :: forall e. Array String -> Boolean -> Boolean -> Eff (process :: PROCESS, fs :: FS, console :: CONSOLE, exception :: EXCEPTION, exception :: EXCEPTION, fs :: FS | e) Unit
app names onlyDeleted generate = case generate of
  true -> do
    e <- exists "project-assets.yml"
    case e of
      true -> log "project-assets.yml already exists."
      false -> do
        createWriteStream "project-assets.yml" >>=
        (\s -> writeString s UTF8 exampleConfig (uncork s)) >>=
        (\result -> case result of
          true -> log "project-assets.yml successfully created."
          false -> log "project-assets.yml could not be created."
        )
  false -> do
    void $ launchAff $ do
      let
        which = case (length names) of
          0 -> All
          1 -> One $ unsafePartial (PartialArray.head names)
          _ -> Many names

      attempt getUserAssets >>= case _ of
        Right groups ->
          case which of
            All -> do
              listAll onlyDeleted groups
            (One name) -> do
              let g = filter (\a -> (groupName a) == name) groups
              case head $ filter (\a -> (groupName a) == name) groups of -- (\(AssetGroup r) -> (r.name == groupName)) groups of
                Just a -> listAll onlyDeleted g
                Nothing -> do
                  Affc.log $ "There is no asset group named '" <> name <> "'"
                  Affc.log $ "Available groups are:"
                  forM (groupNames groups) (\a -> Affc.log $ "  - " <> a)
                  liftEff $ exit 1
            (Many groupNames) -> do
              listAll onlyDeleted $ filter (\(AssetGroup group) ->
                maybe false (const true) (find (eq group.name) groupNames)
              ) groups

        Left err -> do
          Affc.log $ message err --"Could not find project-assets.yml file.  Run `assets -c` to create a new one."

main :: Eff (process :: PROCESS, fs :: FS, console :: CONSOLE, exception :: EXCEPTION, exception :: EXCEPTION, fs :: FS) Unit
main = void $ do
  let
    setup =
      defaultVersion <>
      usage "\n  $0 [-i name] [-m]\n \n Before using, you should create project-assets.yml file in project root and use $0 anywhere above it." <>
      example "$0" "list all assets in all groups" <>
      example "$0 -m" "show missing assets in all groups" <>
      example "$0 -i production" "list all assets in the 'production' group" <>
      example "$0 -i production -i dev -m" "list all missing assets in the 'production' and 'dev' groups" <>
      defaultHelp

  runY setup $
    app <$> yarg "i" ["in"]      (Just "One or more group names to inspect") (Left []) false
        <*> flag "m" ["missing"] (Just "Show only missing assets")
        <*> flag "c" ["create"] (Just "Create an example project-assets.yml file if one doesn't already exist")
