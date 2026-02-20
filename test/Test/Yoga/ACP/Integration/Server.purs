module Test.Yoga.ACP.Integration.Server where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (Foreign)
import Promise (Promise)
import Promise.Aff as Promise

foreign import startServerImpl :: Effect (Promise Int)

startServer :: Aff Int
startServer = Promise.toAffE startServerImpl

foreign import filteredEnvImpl :: EffectFn1 String Foreign

filteredEnv :: String -> Effect Foreign
filteredEnv = runEffectFn1 filteredEnvImpl

foreign import stopServerImpl :: Effect (Promise Unit)

stopServer :: Aff Unit
stopServer = Promise.toAffE stopServerImpl

foreign import isClaudeAvailableImpl :: Effect (Promise Boolean)

isClaudeAvailable :: Aff Boolean
isClaudeAvailable = Promise.toAffE isClaudeAvailableImpl
