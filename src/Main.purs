module Main where

import Prelude
import Assets
import Node.Yargs
import Node.Yargs.Applicative
import Node.Yargs.Setup
import Node.FS as FS
import Node.FS.Async as Async
import Control.Monad.Aff (Aff, attempt, launchAff, makeAff)
import Control.Monad.Aff.Console as Affc
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, foldM, foldMap, reverse, snoc)
import Data.Either (Either(..), either)
import Data.Eq (class Eq)
import Data.Foreign (F, Foreign, readArray, readString, toForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Monoid.Conj (Conj(..))
import Data.Semigroup (class Semigroup)
import Data.Traversable (traverse)
import Data.YAML.Foreign.Decode (parseYAML)
import Node.FS.Aff (stat, FS)
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

--app :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION, fs :: FS | e) Unit
app = void $ launchAff $ do

  Affc.log "---------"
  Affc.log "---------"
  getUserAssets >>= listAll
  Affc.log ""

-- main :: forall e. Eff (exception :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = do
  let
    setup =
      usage "$0" <>
      example "$0 -l" "list all assets in all groups"

  app

  -- runY setup $
  --   app <$> yarg "l" ["list"] (Nothing) (Left []) false
        -- <*> flag "r" []       (Just "Reverse the words")

    -- app <$> yarg "w" ["word"] (Just "A word") (Right "At least one word is required") false
    --     <*> flag "r" []       (Just "Reverse the words")

-- void $ launchAff $ do
  -- listAll Test.tassets
  -- (runExcept readAssets) # either
  --   logShow
  --   logShow
