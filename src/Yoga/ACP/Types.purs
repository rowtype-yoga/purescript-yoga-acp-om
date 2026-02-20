module Yoga.ACP.Types
  ( RunStatus(..)
  , RunMode(..)
  , ErrorCode(..)
  , RunId(..)
  , SessionId(..)
  , AgentName(..)
  , MessagePart
  , Message
  , Run
  , AgentManifest
  , CreateRunRequest
  , ResumeRunRequest
  , ACPError
  , Session
  , ACPEvent(..)
  , Pagination
  , textMessage
  , userMessage
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign (Foreign, ForeignError(..), fail, readString)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Foreign.Index (readProp)

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

data RunStatus
  = Created
  | InProgress
  | Awaiting
  | Cancelling
  | Cancelled
  | Completed
  | Failed

derive instance Eq RunStatus
derive instance Ord RunStatus

instance Show RunStatus where
  show = case _ of
    Created -> "created"
    InProgress -> "in-progress"
    Awaiting -> "awaiting"
    Cancelling -> "cancelling"
    Cancelled -> "cancelled"
    Completed -> "completed"
    Failed -> "failed"

instance WriteForeign RunStatus where
  writeImpl = writeImpl <<< show

instance ReadForeign RunStatus where
  readImpl f = do
    s <- readString f
    case s of
      "created" -> pure Created
      "in-progress" -> pure InProgress
      "awaiting" -> pure Awaiting
      "cancelling" -> pure Cancelling
      "cancelled" -> pure Cancelled
      "completed" -> pure Completed
      "failed" -> pure Failed
      other -> fail $ ForeignError $ "Unknown RunStatus: " <> other

data RunMode = Sync | Async | Stream

derive instance Eq RunMode
derive instance Ord RunMode

instance Show RunMode where
  show = case _ of
    Sync -> "sync"
    Async -> "async"
    Stream -> "stream"

instance WriteForeign RunMode where
  writeImpl = writeImpl <<< show

instance ReadForeign RunMode where
  readImpl f = do
    s <- readString f
    case s of
      "sync" -> pure Sync
      "async" -> pure Async
      "stream" -> pure Stream
      other -> fail $ ForeignError $ "Unknown RunMode: " <> other

data ErrorCode = ServerError | InvalidInput | NotFound

derive instance Eq ErrorCode
derive instance Ord ErrorCode

instance Show ErrorCode where
  show = case _ of
    ServerError -> "server_error"
    InvalidInput -> "invalid_input"
    NotFound -> "not_found"

instance WriteForeign ErrorCode where
  writeImpl = writeImpl <<< show

instance ReadForeign ErrorCode where
  readImpl f = do
    s <- readString f
    case s of
      "server_error" -> pure ServerError
      "invalid_input" -> pure InvalidInput
      "not_found" -> pure NotFound
      other -> fail $ ForeignError $ "Unknown ErrorCode: " <> other

--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

newtype RunId = RunId String
derive instance Newtype RunId _
derive newtype instance Eq RunId
derive newtype instance Ord RunId
derive newtype instance Show RunId
derive newtype instance ReadForeign RunId
derive newtype instance WriteForeign RunId

newtype SessionId = SessionId String
derive instance Newtype SessionId _
derive newtype instance Eq SessionId
derive newtype instance Ord SessionId
derive newtype instance Show SessionId
derive newtype instance ReadForeign SessionId
derive newtype instance WriteForeign SessionId

newtype AgentName = AgentName String
derive instance Newtype AgentName _
derive newtype instance Eq AgentName
derive newtype instance Ord AgentName
derive newtype instance Show AgentName
derive newtype instance ReadForeign AgentName
derive newtype instance WriteForeign AgentName

--------------------------------------------------------------------------------
-- Records
--------------------------------------------------------------------------------

type MessagePart =
  { content_type :: String
  , content :: Maybe String
  , content_url :: Maybe String
  , content_encoding :: Maybe String
  , name :: Maybe String
  , metadata :: Maybe Foreign
  }

type Message =
  { role :: String
  , parts :: Array MessagePart
  }

type Run =
  { agent_name :: Maybe AgentName
  , run_id :: RunId
  , status :: RunStatus
  , output :: Maybe (Array Message)
  , error :: Maybe ACPError
  , session_id :: Maybe SessionId
  , await_request :: Maybe Message
  , created_at :: Maybe String
  , finished_at :: Maybe String
  }

type AgentManifest =
  { name :: AgentName
  , description :: Maybe String
  , input_content_types :: Maybe (Array String)
  , output_content_types :: Maybe (Array String)
  , metadata :: Maybe Foreign
  , status :: Maybe String
  }

type CreateRunRequest =
  { agent_name :: AgentName
  , input :: Array Message
  , mode :: Maybe RunMode
  , session_id :: Maybe SessionId
  }

type ResumeRunRequest =
  { await_resume :: Message
  , mode :: Maybe RunMode
  }

type ACPError =
  { code :: ErrorCode
  , message :: String
  , status :: Maybe Int
  }

type Session =
  { id :: SessionId
  , history :: Maybe (Array Message)
  , state :: Maybe Foreign
  }

type Pagination =
  { limit :: Maybe Int
  , offset :: Maybe Int
  }

--------------------------------------------------------------------------------
-- Events (discriminated union)
--------------------------------------------------------------------------------

data ACPEvent
  = RunCreated Run
  | RunInProgress Run
  | RunAwaiting Run
  | RunCompleted Run
  | RunFailed Run
  | RunCancelled Run
  | MessageCreated Message
  | MessagePart_ MessagePart
  | MessageCompleted Message
  | GenericEvent Foreign
  | ErrorEvent ACPError

instance WriteForeign ACPEvent where
  writeImpl = case _ of
    RunCreated run -> writeTagged "run.created" (writeImpl run)
    RunInProgress run -> writeTagged "run.in-progress" (writeImpl run)
    RunAwaiting run -> writeTagged "run.awaiting" (writeImpl run)
    RunCompleted run -> writeTagged "run.completed" (writeImpl run)
    RunFailed run -> writeTagged "run.failed" (writeImpl run)
    RunCancelled run -> writeTagged "run.cancelled" (writeImpl run)
    MessageCreated msg -> writeTagged "message.created" (writeImpl msg)
    MessagePart_ part -> writeTagged "message.part" (writeImpl part)
    MessageCompleted msg -> writeTagged "message.completed" (writeImpl msg)
    GenericEvent f -> writeTagged "generic" f
    ErrorEvent err -> writeTagged "error" (writeImpl err)
    where
    writeTagged eventType value = writeImpl { event: eventType, data: value }

instance ReadForeign ACPEvent where
  readImpl f = do
    eventType <- readProp "event" f >>= readImpl
    d <- readProp "data" f
    case (eventType :: String) of
      "run.created" -> RunCreated <$> readImpl d
      "run.in-progress" -> RunInProgress <$> readImpl d
      "run.awaiting" -> RunAwaiting <$> readImpl d
      "run.completed" -> RunCompleted <$> readImpl d
      "run.failed" -> RunFailed <$> readImpl d
      "run.cancelled" -> RunCancelled <$> readImpl d
      "message.created" -> MessageCreated <$> readImpl d
      "message.part" -> MessagePart_ <$> readImpl d
      "message.completed" -> MessageCompleted <$> readImpl d
      "error" -> ErrorEvent <$> readImpl d
      _ -> pure $ GenericEvent f


--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

textMessage :: String -> String -> Message
textMessage role content = do
  let part = textPart content
  { role, parts: [ part ] }
  where
  textPart c =
    { content_type: "text/plain"
    , content: Just c
    , content_url: Nothing
    , content_encoding: Nothing
    , name: Nothing
    , metadata: Nothing
    }

userMessage :: String -> Message
userMessage = textMessage "user"
