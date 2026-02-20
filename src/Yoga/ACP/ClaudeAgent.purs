module Yoga.ACP.ClaudeAgent
  ( SDKMessage(..)
  , ResultMsg
  , SystemMsg
  , QueryOptions
  , query
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (Foreign, readString)
import Foreign.Index (readProp)
import Prim.Row (class Union)
import Promise (Promise)
import Promise.Aff as Promise
import Yoga.JSON (class ReadForeign, readImpl)
import Yoga.Om (Om)
import Yoga.Om as Om
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom

--------------------------------------------------------------------------------
-- Opaque types
--------------------------------------------------------------------------------

foreign import data QueryIter :: Type

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import queryImpl :: forall opts. EffectFn1 { | opts } QueryIter

foreign import nextImpl :: EffectFn1 QueryIter (Promise { value :: Nullable Foreign, done :: Boolean })

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

type QueryOptions =
  ( prompt :: String
  , allowedTools :: Array String
  , maxTurns :: Int
  , model :: String
  , cwd :: String
  , permissionMode :: String
  , resume :: String
  , systemPrompt :: String
  , maxBudgetUsd :: Number
  , continue :: Boolean
  , allowDangerouslySkipPermissions :: Boolean
  , env :: Foreign
  )

--------------------------------------------------------------------------------
-- Message types
--------------------------------------------------------------------------------

type ResultMsg =
  { subtype :: String
  , session_id :: String
  , duration_ms :: Number
  , is_error :: Boolean
  , num_turns :: Int
  , total_cost_usd :: Number
  , result :: Maybe String
  }

type SystemMsg =
  { session_id :: String
  , model :: String
  , tools :: Array String
  , cwd :: String
  }

data SDKMessage
  = AssistantMessage { uuid :: String, session_id :: String, message :: Foreign }
  | UserMessage { uuid :: String, session_id :: String, message :: Foreign }
  | ResultMessage ResultMsg
  | SystemMessage SystemMsg
  | StreamEvent { uuid :: String, session_id :: String, event :: Foreign }
  | UnknownMessage Foreign

instance ReadForeign SDKMessage where
  readImpl f = do
    msgType <- readProp "type" f >>= readString
    case msgType of
      "assistant" -> AssistantMessage <$> readImpl f
      "user" -> UserMessage <$> readImpl f
      "result" -> ResultMessage <$> readImpl f
      "system" -> SystemMessage <$> readImpl f
      "stream_event" -> StreamEvent <$> readImpl f
      _ -> pure $ UnknownMessage f

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

query
  :: forall ctx err opts opts_
   . Union opts opts_ QueryOptions
  => { | opts }
  -> Strom ctx err SDKMessage
query opts = initIter # Strom.bindStrom (Strom.unfoldOmStrom pull)
  where
  initIter = Strom.fromOm (runEffectFn1 queryImpl opts # liftEffect)

  pull :: QueryIter -> Om ctx err (Maybe (SDKMessage /\ QueryIter))
  pull it = do
    result <- runEffectFn1 nextImpl it # Promise.toAffE # Om.toOm
    case Nullable.toMaybe result.value of
      Nothing -> pure Nothing
      Just raw -> pure $ Just (parseMessage raw /\ it)

  parseMessage :: Foreign -> SDKMessage
  parseMessage raw = case runExcept (readImpl raw) of
    Left _ -> UnknownMessage raw
    Right msg -> msg
