module Yoga.ACP.Client
  ( ping
  , listAgents
  , listAgentsPage
  , getAgent
  , createRun
  , getRun
  , cancelRun
  , resumeRun
  , listRunEvents
  , getSession
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import JS.Fetch as Fetch
import JS.Fetch.Headers as Headers
import JS.Fetch.Request as Request
import JS.Fetch.RequestBody as Body
import JS.Fetch.RequestCredentials as Credentials
import JS.Fetch.RequestMode as Mode
import JS.Fetch.Referrer as Referrer
import JS.Fetch.ReferrerPolicy as ReferrerPolicy
import JS.Fetch.Integrity (Integrity(..))
import JS.Fetch.Duplex as Duplex
import JS.Fetch.RequestCache as Cache
import JS.Fetch.Response as Resp
import Promise.Aff as Promise
import Data.String as String
import Unsafe.Coerce (unsafeCoerce)
import Data.Newtype (un)
import Yoga.ACP.SSE as SSE
import Yoga.ACP.Types (ACPEvent(..), ACPError, AgentManifest, AgentName(..), CreateRunRequest, ErrorCode(..), Pagination, ResumeRunRequest, Run, RunId(..), RunMode(..), Session, SessionId(..))
import Yoga.JSON (class ReadForeign, readJSON, writeJSON)
import Yoga.JSON.Error (withStringErrors)
import Yoga.Om (Om, ask, throw)
import Yoga.Om as Om
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom
import Yoga.Om.Strom.WebStream as WebStream

foreign import decodeUtf8 :: Uint8Array -> String

--------------------------------------------------------------------------------
-- Health
--------------------------------------------------------------------------------

ping
  :: forall ctx err
   . Om { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) Unit
ping = do
  baseUrl <- getBaseUrl
  _ <- jsonGet @{} (baseUrl <> "/ping")
  pure unit

--------------------------------------------------------------------------------
-- Discovery
--------------------------------------------------------------------------------

listAgents
  :: forall ctx err
   . Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) AgentManifest
listAgents = Strom.fromOm fetchAgents # Strom.bindStrom Strom.fromArray
  where
  fetchAgents = do
    baseUrl <- getBaseUrl
    jsonGet @(Array AgentManifest) (baseUrl <> "/agents")

listAgentsPage
  :: forall ctx err
   . Pagination
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) AgentManifest
listAgentsPage page = Strom.fromOm fetchAgents # Strom.bindStrom Strom.fromArray
  where
  fetchAgents = do
    baseUrl <- getBaseUrl
    jsonGet @(Array AgentManifest) (baseUrl <> "/agents" <> paginationQuery page)

getAgent
  :: forall ctx err
   . AgentName
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) AgentManifest
getAgent name = Strom.fromOm do
  baseUrl <- getBaseUrl
  jsonGet @AgentManifest (baseUrl <> "/agents/" <> un AgentName name)

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

createRun
  :: forall ctx err
   . CreateRunRequest
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) ACPEvent
createRun req = streamRun POST "/runs" body
  where
  body = writeJSON (req { mode = Just Stream })

resumeRun
  :: forall ctx err
   . RunId
  -> ResumeRunRequest
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) ACPEvent
resumeRun runId req = streamRun POST ("/runs/" <> un RunId runId <> "/resume") body
  where
  body = writeJSON (req { mode = Just Stream })

getRun
  :: forall ctx err
   . RunId
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) Run
getRun runId = Strom.fromOm do
  baseUrl <- getBaseUrl
  jsonGet @Run (baseUrl <> "/runs/" <> un RunId runId)

cancelRun
  :: forall ctx err
   . RunId
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) Run
cancelRun runId = Strom.fromOm do
  baseUrl <- getBaseUrl
  jsonPost @Run (baseUrl <> "/runs/" <> un RunId runId <> "/cancel") Nothing

listRunEvents
  :: forall ctx err
   . RunId
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) ACPEvent
listRunEvents runId = Strom.fromOm fetchEvents # Strom.bindStrom Strom.fromArray
  where
  fetchEvents = do
    baseUrl <- getBaseUrl
    jsonGet @(Array ACPEvent) (baseUrl <> "/runs/" <> un RunId runId <> "/events")

--------------------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------------------

getSession
  :: forall ctx err
   . SessionId
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) Session
getSession sessionId = Strom.fromOm do
  baseUrl <- getBaseUrl
  jsonGet @Session (baseUrl <> "/sessions/" <> un SessionId sessionId)

--------------------------------------------------------------------------------
-- Streaming internals
--------------------------------------------------------------------------------

streamRun
  :: forall ctx err
   . Method
  -> String
  -> String
  -> Strom { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) ACPEvent
streamRun method path reqBody = Strom.mkStrom do
  baseUrl <- getBaseUrl
  let url = baseUrl <> path
  resp <- fetchRaw method sseHeaders url (Just reqBody)
  let status = Resp.status resp
  when (status < 200 || status >= 300) do
    respText <- Promise.toAffE (Resp.text resp) # Om.toOm
    throwACPError status respText
  webStream <- Resp.body resp # liftEffect
  let stromStream = WebStream.fromReadableStreamWith { chunkSize: 1 } (unsafeCoerce webStream)
  let textStream = Strom.mapStrom decodeUtf8 stromStream
  let eventStream = SSE.parseSSE textStream
  let acpStream = Strom.collectStrom parseACPEvent eventStream
  Strom.runStrom acpStream
  where
  sseHeaders = Headers.fromFoldable
    [ "Accept" /\ "text/event-stream"
    , "Content-Type" /\ "application/json"
    ]

parseACPEvent :: SSE.SSEvent -> Maybe ACPEvent
parseACPEvent sse = case sse.event of
  Just "run.created" -> decodeData RunCreated sse."data"
  Just "run.in-progress" -> decodeData RunInProgress sse."data"
  Just "run.awaiting" -> decodeData RunAwaiting sse."data"
  Just "run.completed" -> decodeData RunCompleted sse."data"
  Just "run.failed" -> decodeData RunFailed sse."data"
  Just "run.cancelled" -> decodeData RunCancelled sse."data"
  Just "message.created" -> decodeData MessageCreated sse."data"
  Just "message.part" -> decodeData MessagePart_ sse."data"
  Just "message.completed" -> decodeData MessageCompleted sse."data"
  Just "error" -> decodeData ErrorEvent sse."data"
  _ -> Nothing
  where
  decodeData :: forall a. ReadForeign a => (a -> ACPEvent) -> String -> Maybe ACPEvent
  decodeData ctor jsonStr = case withStringErrors (readJSON jsonStr) of
    Right val -> Just (ctor val)
    Left _ -> Nothing

--------------------------------------------------------------------------------
-- HTTP helpers
--------------------------------------------------------------------------------

getBaseUrl :: forall ctx err. Om { acpBaseUrl :: String | ctx } err String
getBaseUrl = _.acpBaseUrl <$> ask

jsonGet
  :: forall @a ctx err
   . ReadForeign a
  => String
  -> Om { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) a
jsonGet url = do
  resp <- fetchRaw GET jsonHeaders url Nothing
  respText <- Promise.toAffE (Resp.text resp) # Om.toOm
  let status = Resp.status resp
  if status >= 200 && status < 300 then
    case withStringErrors (readJSON @a respText) of
      Right body -> pure body
      Left errs -> throwACPError status ("Failed to parse response:\n" <> errs <> "\nBody: " <> respText)
  else
    throwACPError status respText

jsonPost
  :: forall @a ctx err
   . ReadForeign a
  => String
  -> Maybe String
  -> Om { acpBaseUrl :: String | ctx } (acpError :: ACPError | err) a
jsonPost url maybeBody = do
  resp <- fetchRaw POST jsonHeaders url maybeBody
  respText <- Promise.toAffE (Resp.text resp) # Om.toOm
  let status = Resp.status resp
  if status >= 200 && status < 300 then
    case withStringErrors (readJSON @a respText) of
      Right body -> pure body
      Left errs -> throwACPError status ("Failed to parse response:\n" <> errs <> "\nBody: " <> respText)
  else
    throwACPError status respText

fetchRaw
  :: forall ctx err
   . Method
  -> Headers.Headers
  -> String
  -> Maybe String
  -> Om ctx err Resp.Response
fetchRaw method headers url maybeBody = do
  let reqBody = case maybeBody of
        Nothing -> Body.empty
        Just b -> Body.fromString b
  let options =
        { method
        , headers
        , body: reqBody
        , credentials: Credentials.SameOrigin
        , mode: Mode.Cors
        , referrer: Referrer.ReferrerUrl ""
        , referrerPolicy: ReferrerPolicy.NoReferrer
        , integrity: Integrity ""
        , duplex: Duplex.Half
        , cache: Cache.Default
        }
  request <- Request.new url options # liftEffect
  Promise.toAffE (Fetch.fetch request) # Om.toOm

jsonHeaders :: Headers.Headers
jsonHeaders = Headers.fromFoldable
  [ "Content-Type" /\ "application/json"
  , "Accept" /\ "application/json"
  ]

throwACPError
  :: forall ctx err a
   . Int
  -> String
  -> Om ctx (acpError :: ACPError | err) a
throwACPError status body = case withStringErrors (readJSON @ACPError body) of
  Right acpErr -> throw { acpError: acpErr }
  Left _ -> throw { acpError: { code: ServerError, message: body, status: Just status } }

paginationQuery :: Pagination -> String
paginationQuery { limit, offset } = do
  let params = Array.catMaybes
        [ limit <#> \l -> "limit=" <> show l
        , offset <#> \o -> "offset=" <> show o
        ]
  case params of
    [] -> ""
    _ -> "?" <> String.joinWith "&" params
