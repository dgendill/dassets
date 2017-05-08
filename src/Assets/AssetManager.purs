module Assets.AssetManager where

import Prelude
import Assets.AssetPath
import Assets.AssetGroup
import Control.Monad.Aff.Console as Affc
import Assets.AssetGroup (AssetGroup(..))
import Control.Monad.Aff (Aff, attempt, launchAff, makeAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION, error, throw)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, throwError)
import Data.Array (find, foldM, foldMap, snoc)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Foreign (F, Foreign, readArray, readString, renderForeignError, toForeign)
import Data.Foreign.Index (readProp)
import Data.Generic (class Generic, gEq, gShow)
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.YAML.Foreign.Decode (parseYAML)
import Node.FS.Aff (stat, FS)
import Test.Spec.Color (Color(..), colored)
import Util (forM, traverseFindDeep, whenTrue)

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


getUserAssets :: forall e. Aff (exception :: EXCEPTION, fs :: FS | e) AssetManager
getUserAssets = do
  file <- traverseFindDeep 5 "./" "project-assets.yml"
  case (runExcept $ (parseYAML file) >>= readAssetManager) of
    Right am -> pure am
    Left err -> throwError $ error $ "Could not parse project-assets.yml: " <> (foldMap renderForeignError (toList err))

readAssetPath :: Foreign -> F AssetPath
readAssetPath f = readString f >>= (pure <<< File)

readAssetGroup :: Foreign -> F AssetGroup
readAssetGroup f = do
  name <- readProp "name" f >>= readString
  list <- readProp "paths" f >>= readArray >>= traverse readAssetPath
  pure $ AssetGroup {name, list}

readAssetManager :: Foreign -> F AssetManager
readAssetManager f = readArray f >>= traverse readAssetGroup


  -- case r of
  --   Right fileContent ->
  --   Left e -> throw e
