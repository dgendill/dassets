module Test.AssetManager where

import Prelude
import Assets
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Aff.Console (logShow)

main = mainVisual

mainVisual = void $ launchAff $ do
  a <- attempt $ getUserAssets
  logShow a
