module Assets.AssetManager where

import Prelude
import Assets.AssetPath
import Assets.AssetGroup
import Node.FS as FS
import Node.FS.Async as Async
import Assets.AssetGroup (AssetGroup(..))
import Control.Monad.Aff (Aff, attempt, launchAff, makeAff, runAff)
import Control.Monad.Aff.Console as Affc
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, foldM, foldMap, snoc)
import Data.Either (Either(..), either)
import Data.Eq (class Eq)
import Data.Foldable (foldl)
import Data.Foreign (F, Foreign, readArray, readString, toForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid)
import Data.Monoid.Conj (Conj(..))
import Data.Semigroup (class Semigroup)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.YAML.Foreign.Decode (parseYAML)
import Node.FS.Aff (stat, FS)
import Test.Spec.Color (Color(..), colored)


type AssetManager = Array AssetGroup

addTo :: AssetGroup -> AssetPath -> AssetGroup
addTo (AssetGroup { name, list }) a = AssetGroup { name : name, list : snoc list a }

assetGroup :: String -> Array AssetPath -> AssetGroup
assetGroup name list = AssetGroup { name, list }

findAsset :: String -> AssetManager -> Maybe AssetGroup
findAsset needle haystack = find (\(AssetGroup { name, list }) -> name == needle) haystack

assetsGroupExists :: forall e. AssetGroup -> Aff (fs :: FS | e) Boolean
assetsGroupExists (AssetGroup {list}) =
  (either id id) <$> (runExceptT $ foldM doesExist true list)
  where
    doesExist :: forall ee. Boolean -> AssetPath -> ExceptT Boolean (Aff (fs :: FS | ee)) Boolean
    doesExist name path = do
      a <- liftAff $ assetPathExists path
      if a == false
        then except $ Left false
        else except $ Right true


assetPathExists :: forall e. AssetPath -> Aff (fs :: FS | e) Boolean
assetPathExists  = case _ of
  (File path) -> checkExists path
  (Dir path)  -> checkExists path
  where
    checkExists path = pure <<< either (const false) (const true) =<< attempt (stat path)

forM :: forall a m. (Monad m) => Array a -> (a -> m Unit) -> m Unit
forM v fn = foldM (\a v' -> fn v') unit v

listAll :: forall e. AssetManager -> Aff (fs :: FS, console :: CONSOLE | e) Unit
listAll manager =
  forM manager (\(AssetGroup {name, list}) -> do
    Affc.log $ "- " <> name
    forM list (\path -> do
      exists <- (assetPathExists path)

      let pre = if exists then "" else "missing "
      Affc.log $ (whenTrue exists (colored Green) (colored Fail)) $ case path of
        Dir  d -> "  " <> pre <> "dir:" <> d
        File f -> "  " <> pre <> "fil:" <> f
    )
    Affc.log $ ""
  )

whenTrue :: forall a. Boolean -> a -> a -> a
whenTrue isTrue tFn fFn =
  if isTrue
    then tFn
    else fFn
