module Test.Util where

import Prelude
import Assets
import Util.General
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (isLeft, isRight)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

-- mainTest :: forall e. Eff (RunnerEffects _) Unit
mainTests = do
  it "can traverse the file tree" $ do
    notThere <- attempt $ traverseFind "./" "noteawefs.txt"
    (isLeft notThere) `shouldEqual` true

    traverseFind "./" "testfile.txt" >>=
      (\r -> (trim r) `shouldEqual` "test")
