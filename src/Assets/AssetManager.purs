module Assets.AssetManager where

import Prelude
import Control.Monad.Aff.Console as Affc
import Assets.AssetGroup (AssetGroup(..), foldMAssetGroup, groupName)
import Assets.AssetPath (AssetPath(..), assetPathExists)
import Control.Alt (alt, (<|>))
import Control.Lazy (class Lazy)
import Control.Monad.Aff (Aff, attempt, launchAff, makeAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION, error, throw, throwException)
import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, throwError)
import Data.Argonaut (Json, decodeJson, toArray)
import Data.Array (find, foldM, foldMap, length, snoc)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, sequence_, traverse_)
import Data.Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, readArray, readString, renderForeignError, toForeign)
import Data.Foreign.Index (readProp)
import Data.Generic (class Generic, gEq, gShow)
import Data.Int (toStringAs)
import Data.Lazy (Lazy, force, defer)
import Data.List.Lazy (List(..), singleton)
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.YAML.Foreign.Decode (parseYAMLToJson)
import Node.FS.Aff (stat, FS)
import Unsafe.Coerce (unsafeCoerce)
import Util.Color (Color(..), colored)
import Util.General (forM, meither, traverseFindDeep, whenTrue)

type AssetManager = Array AssetGroup

groupNames :: AssetManager -> Array String
groupNames = foldMap (\n -> [groupName n])

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


type GroupStatus e = {
  io :: (Array (Lazy (Aff (fs :: FS, console :: CONSOLE | e) Unit))),
  all :: Int,
  missing :: Int,
  found :: Int
}

listOne :: forall e. Boolean -> AssetGroup -> Aff (fs :: FS, console :: CONSOLE | e) (GroupStatus e)
listOne onlyDeleted (ag@AssetGroup {name, list}) = do
  foldMAssetGroup (\status path -> do
    exists <- assetPathExists path

    case Tuple exists onlyDeleted of
      (Tuple true false) -> do
        let fn = defer \_ -> Affc.log $ colored Green $ case path of
                   Dir  d -> "  dir:" <> d
                   File f -> "  fil:" <> f

        pure (status {
            io = snoc status.io fn,
            found = status.found + 1,
            all = status.all + 1
          })
      (Tuple false _) -> do
        let fn = defer \_ -> Affc.log $ colored Fail $ case path of
                   Dir  d -> "  missing dir:" <> d
                   File f -> "  missing fil:" <> f

        pure (status {
          io = snoc status.io fn,
          missing = status.missing + 1,
          all = status.all + 1
        })
      _ -> pure (status { all = status.all + 1})

    ) { all : 0, missing : 0, found : 0, io : [] } ag


listAll :: forall e. Boolean -> AssetManager -> Aff (fs :: FS, console :: CONSOLE | e) Unit
listAll onlyDeleted manager =
  forM manager (\ag@(AssetGroup {name, list}) -> do
    status <- listOne onlyDeleted ag

    case Tuple status onlyDeleted of
      (Tuple { all : 0 }        _) -> Affc.log $ "There are no assets in group '" <> name <> "'"
      (Tuple { missing : 0 } true) ->
        if length manager == 1
          then Affc.log $ (colored Green) "  OK." <> " Group '" <> name <> "' has all assets"
          else pure unit
      _ -> do
        Affc.log $ "- " <> name
        traverse_ force status.io

  )

jsonToAssetManager :: Json -> Either String AssetManager
jsonToAssetManager json =
  (meither (toArray json) "AssetManager must be an array of AssetGroups.") >>=
  traverse decodeJson

renderMultipleErrors :: MultipleErrors -> String
renderMultipleErrors = foldMap renderForeignError <<< toList

getUserAssets :: forall e. Aff (exception :: EXCEPTION, fs :: FS | e) AssetManager
getUserAssets = do
  file <- traverseFindDeep 5 "./" "project-assets.yml"
  case (runExcept $ (parseYAMLToJson file)) of
    Left err -> throwError $ error $ "Could not parse project-assets.yml: "
    Right json -> either (throwError <<< error) pure (jsonToAssetManager json)
