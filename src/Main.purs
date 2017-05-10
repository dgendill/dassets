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
import Control.Alternative (empty)
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
import Node.FS.Aff (stat, FS)
import Node.Process (PROCESS, exit)
import Node.Yargs (Yargs)
import Node.Yargs.Applicative (Y, flag, rest, runY, yarg)
import Node.Yargs.Setup (YargsSetup, example, usage, string)
import Partial.Unsafe (unsafePartial)
import Util.General (forM)
-- import Test.Main as Test

setup :: String
setup = """
  - shared
  - personal
  - advertising
  - android
"""

readAssets :: F (Array String)
readAssets = (traverse readString) =<< readArray =<< parseYAML setup


data Which = All | One String | Many (Array String)

-- l :: List (Array Int)
-- l = defer (\_ -> singleton [1,2,55])

-- some :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
--
-- some accepts an f a where f has an alternative and lazy instance.  The only one I can think
-- of that would be relevant is List.  So I'm going to pass a List Int to `some`, and somehow I
-- get back List (Array Int).  How do I determine how some
--
-- How do I use Array.some.  All of the examples I've found are used in conjunction with a Parser.
-- The end goal is to implement a lazy `contains` function for arrays.  e.g.
--
-- [1,2,3,4,5] `contains`

--
-- l :: List Int
-- l = fromFoldable [1,2,3,4,5,6]
--
-- firstGtThan :: Int -> Maybe Int
-- firstGtThan v = do
--   a <- some (Just 5)
--   if (a > 11)
--     then pure 5
--     else empty

app :: forall e. Array String -> Boolean -> Eff (process :: PROCESS, fs :: FS, console :: CONSOLE, exception :: EXCEPTION, exception :: EXCEPTION, fs :: FS | e) Unit
app names onlyDeleted = void $ launchAff $ do -- runAff logShow logShow $ do
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
      Affc.log $ message err


testt :: forall a. Array Foreign -> Array String
testt a = case runExcept (traverse readString a) of
  Left _ -> []
  Right a -> a

-- readWhich :: Foreign -> F Which
-- readWhich f = readString >>= (case _ of
--   "")
--
-- instance argWhich :: Arg Which where
--   arg key = Y { setup: string key
--               , read: readOneOrMany readWhich key
--               }

main = void $ do
  let
    setup =
      usage "$0" <>
      example "$0 -l" "list all assets in all groups"

  runY setup $
    app <$> yarg "i" ["in"]      (Just "The group name to inspect") (Left []) false --yarg "i" ["in"] (Nothing) (Left []) false)
        <*> flag "m" ["missing"] (Just "Show only missing assets") -- (Left "") false --yarg "i" ["in"] (Nothing) (Left []) false)

    -- app <$> yarg "w" ["word"] (Just "A word") (Right "At least one word is required") false
    --     <*> flag "r" []       (Just "Reverse the words")

-- void $ launchAff $ do
  -- listAll Test.tassets
  -- (runExcept readAssets) # either
  --   logShow
  --   logShow
