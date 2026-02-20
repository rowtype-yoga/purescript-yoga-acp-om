module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Config (defaultConfig)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.Yoga.ACP.Types.Spec as Types.Spec
import Test.Yoga.ACP.SSE.Spec as SSE.Spec
import Test.Yoga.ACP.Integration.Spec as Integration.Spec

main :: Effect Unit
main = launchAff_ $ runSpec' config [consoleReporter] do
  Types.Spec.spec
  SSE.Spec.spec
  Integration.Spec.spec
  where
    config = defaultConfig { timeout = Just (Milliseconds 60000.0) }
