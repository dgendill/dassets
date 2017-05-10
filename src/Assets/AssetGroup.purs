module Assets.AssetGroup where

import Prelude
import Assets.AssetPath (AssetPath, assetPathExists)
import Control.Monad.Aff (Aff)
import Data.Argonaut (class DecodeJson, decodeJson, getField, toObject)
import Data.Array (foldM, snoc)
import Data.Generic (class Generic, gEq)
import Data.Traversable (traverse)
import Node.FS.Aff (FS)
import Util.General (failWith, maybeFail)


newtype AssetGroup = AssetGroup {
  name :: String,
  list :: Array AssetPath
}

derive instance gAssetG :: Generic AssetGroup
instance showAssetG :: Show AssetGroup where show (AssetGroup {name, list}) = name <> " (" <> show list <>")"
instance eqAssetG :: Eq AssetGroup where eq = gEq

instance semigroup :: Semigroup AssetGroup where
  append (AssetGroup {name : name1, list : list1}) (AssetGroup {name : name2, list : list2}) =
    AssetGroup {
      name : name1, -- if name1 /= "" then name1 else name2,
      list : list1 <> list2
    }

instance decodeAssetGroup :: DecodeJson AssetGroup where
  decodeJson json = do
    obj <- maybeFail "AssetGroup must be an object." (toObject json)
    name <- failWith "Could not find property 'name' on AssetGroup." $ getField obj "name"
    paths <- failWith "Could not find property 'paths' on AssetGroup." $ (getField obj "paths")
    list <- traverse decodeJson paths
    pure $ AssetGroup {name, list}

addTo :: AssetGroup -> AssetPath -> AssetGroup
addTo (AssetGroup { name, list }) a = AssetGroup { name : name, list : snoc list a }

assetGroup :: String -> Array AssetPath -> AssetGroup
assetGroup name list = AssetGroup { name, list }

groupName :: AssetGroup -> String
groupName (AssetGroup {name}) = name

groupList :: AssetGroup -> Array AssetPath
groupList (AssetGroup {list}) = list

foldMAssetGroup :: forall a m. (Monad m) => (a -> AssetPath -> m a) -> a -> AssetGroup -> m a
foldMAssetGroup fn acc (AssetGroup {name, list}) = foldM fn acc list


foldMissing :: forall e a
   . (a -> AssetPath -> Aff (fs :: FS | e) a)
   -> a -> AssetGroup -> Aff (fs :: FS | e) a
foldMissing fn = foldMAssetGroup \acc path -> do
    exists <- (assetPathExists path)
    if (exists)
      then fn acc path
      else pure acc
