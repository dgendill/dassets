module Assets.AssetPath where

import Prelude
import Data.Generic (class Generic, gEq, gShow)

data AssetPath
  = File String
  | Dir String

derive instance gAssetPath :: Generic AssetPath
instance showAssetPath :: Show AssetPath where
  show (File f) = f
  show (Dir f)  = f

instance eqAssetPath :: Eq AssetPath where eq = gEq
