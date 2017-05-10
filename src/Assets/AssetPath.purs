module Assets.AssetPath where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut (class DecodeJson, toString)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Generic (class Generic, gEq, gShow)
import Node.FS.Aff (FS, stat)
import Util.General (meither)

data AssetPath
  = File String
  | Dir String

derive instance gAssetPath :: Generic AssetPath
instance showAssetPath :: Show AssetPath where
  show (File f) = f
  show (Dir f)  = f

instance eqAssetPath :: Eq AssetPath where eq = gEq

instance decodeAssetPath :: DecodeJson AssetPath where
  decodeJson json =
    lmap ("fail: " <> _) (meither (toString json) "a" >>= (pure <<< File))


assetPathExists :: forall e. AssetPath -> Aff (fs :: FS | e) Boolean
assetPathExists  = case _ of
  (File path) -> checkExists path
  (Dir path)  -> checkExists path
  where
    checkExists path = pure <<< either (const false) (const true) =<< attempt (stat path)
