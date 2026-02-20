module Test.Yoga.ACP.Integration.Spec where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Yoga.ACP.Integration.Server (filteredEnv, isClaudeAvailable, startServer, stopServer)
import Yoga.ACP.Client (createRun, listAgents, listAgentsPage, ping)
import Yoga.ACP.ClaudeAgent as Agent
import Yoga.ACP.Store (initStore, storeInputMessages, tapStore)
import Yoga.ACP.Types (ACPEvent(..), AgentName(..), Message, userMessage)
import Yoga.JSON (read_)
import Yoga.Om (Om)
import Yoga.Om as Om
import Yoga.Om.Strom as Strom
import Yoga.SQLite.SQLite as SQLite

spec :: Spec Unit
spec = describe "Integration" do
  pingSpec
  listAgentsSpec
  listAgentsPageSpec
  createRunSpec
  storeSpec
  agentSDKSpec

pingSpec :: Spec Unit
pingSpec = it "pings the ACP server" do
  available <- isClaudeAvailable
  when available do
    port <- startServer
    result <- runWithPort port ping
    stopServer
    case result of
      Right _ -> pure unit
      Left _ -> fail "ping failed"

listAgentsSpec :: Spec Unit
listAgentsSpec = it "lists agents from Claude Code" do
  available <- isClaudeAvailable
  when available do
    port <- startServer
    result <- runWithPort port (Strom.runCollect listAgents)
    stopServer
    case result of
      Right agents -> do
        Array.length agents `shouldEqual` 1
        (Array.head agents <#> _.name) `shouldEqual` Just (AgentName "claude-code")
      Left _ ->
        fail "expected agents but got error"

listAgentsPageSpec :: Spec Unit
listAgentsPageSpec = it "lists agents with pagination" do
  available <- isClaudeAvailable
  when available do
    port <- startServer
    result <- runWithPort port (Strom.runCollect (listAgentsPage { limit: Just 1, offset: Nothing }))
    stopServer
    case result of
      Right agents -> do
        Array.length agents `shouldEqual` 1
        (Array.head agents <#> _.name) `shouldEqual` Just (AgentName "claude-code")
      Left _ ->
        fail "expected agents but got error"

createRunSpec :: Spec Unit
createRunSpec = it "sends a message and receives streaming events" do
  available <- isClaudeAvailable
  when available do
    port <- startServer
    let req =
          { agent_name: AgentName "claude-code"
          , input: [ userMessage "Reply with exactly the word 'hello'" ]
          , mode: Nothing
          , session_id: Nothing
          }
    result <- runWithPort port (Strom.runCollect (createRun req))
    stopServer
    case result of
      Right events -> do
        any isRunCreated events `shouldEqual` true
        any isRunCompleted events `shouldEqual` true
        let messages = Array.mapMaybe completedMessage events
        let text = Array.head messages >>= messageText # fromMaybe ""
        text `shouldSatisfy` (not <<< String.contains (String.Pattern "Error"))
      Left _ ->
        fail "expected events but got error"

runWithPort :: forall err a. Int -> Om { acpBaseUrl :: String } err a -> Aff (Either (Variant (exception :: Error | err)) a)
runWithPort port om =
  Om.runReader { acpBaseUrl: "http://localhost:" <> show port } om

isRunCreated :: ACPEvent -> Boolean
isRunCreated (RunCreated _) = true
isRunCreated _ = false

isRunCompleted :: ACPEvent -> Boolean
isRunCompleted (RunCompleted _) = true
isRunCompleted _ = false

completedMessage :: ACPEvent -> Maybe Message
completedMessage (MessageCompleted m) = Just m
completedMessage _ = Nothing

messageText :: Message -> Maybe String
messageText msg = Array.head msg.parts >>= _.content

type MessageRow =
  { run_id :: String
  , role :: String
  , content :: Maybe String
  }

storeSpec :: Spec Unit
storeSpec = it "stores streamed messages in SQLite" do
  available <- isClaudeAvailable
  when available do
    port <- startServer
    conn <- SQLite.sqlite { url: "file:store-test.db" } # liftEffect
    let runId = "test-run-1"
    let input = [ userMessage "Reply with exactly the word 'hello'" ]
    let req =
          { agent_name: AgentName "claude-code"
          , input
          , mode: Nothing
          , session_id: Nothing
          }
    let pipeline = do
          initStore conn
          void $ SQLite.execute (SQLite.SQL "DELETE FROM messages WHERE run_id = ?") [ SQLite.toSQLiteValue runId ] conn # Om.toOm
          storeInputMessages conn runId input
          createRun req # tapStore conn runId # Strom.runCollect
    result <- runWithPort port pipeline
    stopServer
    case result of
      Right _ -> do
        qr <- SQLite.query (SQLite.SQL "SELECT run_id, role, content FROM messages WHERE run_id = ?") [ SQLite.toSQLiteValue runId ] conn
        let rows = Array.mapMaybe read_ qr.rows
        Array.length rows `shouldEqual` 2
        any (\(r :: MessageRow) -> r.role == "user") rows `shouldEqual` true
        any (\(r :: MessageRow) -> r.role == "assistant") rows `shouldEqual` true
        SQLite.close conn
      Left _ -> do
        SQLite.close conn
        fail "expected events but got error"

agentSDKSpec :: Spec Unit
agentSDKSpec = it "streams messages via Claude Agent SDK" do
  available <- isClaudeAvailable
  when available do
    env <- filteredEnv "CLAUDE" # liftEffect
    let stream = Agent.query
          { prompt: "Reply with exactly the word 'hello'"
          , maxTurns: 1
          , permissionMode: "bypassPermissions"
          , allowDangerouslySkipPermissions: true
          , env
          }
    result <- Om.runReader {} (Strom.runCollect stream)
    case result of
      Right msgs -> do
        Array.length msgs `shouldSatisfy` (_ >= 1)
        any isResult msgs `shouldEqual` true
      Left _ ->
        fail "expected SDK messages but got error"

isResult :: Agent.SDKMessage -> Boolean
isResult (Agent.ResultMessage _) = true
isResult _ = false
