module Test.Main where

import Prelude
import Assets
import Test.AssetManager as AssetManagerTest
import Test.Util as Util
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (isRight)
import Data.Maybe (Maybe(..))
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: forall e. Eff (RunnerEffects _) Unit
main = run [consoleReporter] do
  it "can lookup assets in AssetManager" $ do
    (Just shared) `shouldEqual` (findAsset "shared" tassets)
    Nothing `shouldEqual` (findAsset "notthere" tassets)
  it "can find individual AssetPaths" $ do
    assetPathExists dirAssetPath      >>= shouldEqual true
    assetPathExists (Dir "not there") >>= shouldEqual false
  it "can determine if AssetGroups exist" $ do
    assetsGroupExists shared >>= shouldEqual true
    assetsGroupExists missingFileAssetGroup >>= shouldEqual false

  it "can read the project-assets.yml file" $ do
    a <- attempt $ getUserAssets
    (isRight a) `shouldEqual` true

  Util.mainTests


dirAssetPath :: AssetPath
dirAssetPath = Dir "test/files/js/"

shared :: AssetGroup
shared = assetGroup "shared" [
    Dir "test/files/images",
    dirAssetPath
  ]

missingFileAssetGroup :: AssetGroup
missingFileAssetGroup = assetGroup "shared" [
    Dir "test/files/images",
    Dir "not there",
    Dir "test/files/images"
  ]

tassets :: AssetManager
tassets = [
  shared,
  assetGroup "production" [
    File "test/files/production.txt"
  ],
  assetGroup "dev" [
    File "test/files/dev.txt"
  ]
]
