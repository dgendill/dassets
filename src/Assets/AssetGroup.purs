module Assets.AssetGroup where

import Prelude
import Node.FS as FS
import Node.FS.Async as Async
import Control.Monad.Aff (Aff, attempt, launchAff, makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, foldM, foldMap, snoc)
import Data.Either (Either(..), either)
import Data.Eq (class Eq)
import Data.Foreign (F, Foreign, readArray, readString, toForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Data.Monoid.Conj (Conj(..))
import Data.Semigroup (class Semigroup)
import Data.Traversable (traverse)
import Data.YAML.Foreign.Decode (parseYAML)
import Node.FS.Aff (stat, FS)
import Assets.AssetPath (AssetPath(..))


newtype AssetGroup = AssetGroup {
  name :: String,
  list :: Array AssetPath
}

derive instance gAssetG :: Generic AssetGroup
instance showAssetG :: Show AssetGroup where show (AssetGroup {name}) = name
instance eqAssetG :: Eq AssetGroup where eq = gEq

-- instance monoidAssetG :: Monoid AssetGroup where
  -- mempty = (AssetGroup { name : "", list : []})

instance semigroup :: Semigroup AssetGroup where
  append (AssetGroup {name : name1, list : list1}) (AssetGroup {name : name2, list : list2}) =
    AssetGroup {
      name : name1, -- if name1 /= "" then name1 else name2,
      list : list1 <> list2
    }
