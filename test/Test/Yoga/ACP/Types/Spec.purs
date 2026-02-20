module Test.Yoga.ACP.Types.Spec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Yoga.ACP.Types (RunStatus(..), RunMode(..), ErrorCode(..), ACPEvent(..), RunId(..), AgentName(..), textMessage, userMessage)
import Yoga.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Yoga.JSON.Error (withStringErrors)

spec :: Spec Unit
spec = describe "Yoga.ACP.Types" do
  runStatusSpec
  runModeSpec
  errorCodeSpec
  messageSpec
  runSpec
  acpEventSpec

runStatusSpec :: Spec Unit
runStatusSpec = describe "RunStatus" do
  it "roundtrips Created" do
    roundtrip Created
  it "roundtrips InProgress" do
    roundtrip InProgress
  it "roundtrips Awaiting" do
    roundtrip Awaiting
  it "roundtrips Cancelling" do
    roundtrip Cancelling
  it "roundtrips Cancelled" do
    roundtrip Cancelled
  it "roundtrips Completed" do
    roundtrip Completed
  it "roundtrips Failed" do
    roundtrip Failed
  it "serializes to wire format" do
    writeJSON InProgress `shouldEqual` "\"in-progress\""
  it "serializes Created to wire format" do
    writeJSON Created `shouldEqual` "\"created\""

runModeSpec :: Spec Unit
runModeSpec = describe "RunMode" do
  it "roundtrips Sync" do
    roundtrip Sync
  it "roundtrips Async" do
    roundtrip Async
  it "roundtrips Stream" do
    roundtrip Stream
  it "serializes to wire format" do
    writeJSON Stream `shouldEqual` "\"stream\""

errorCodeSpec :: Spec Unit
errorCodeSpec = describe "ErrorCode" do
  it "roundtrips ServerError" do
    roundtrip ServerError
  it "roundtrips InvalidInput" do
    roundtrip InvalidInput
  it "roundtrips NotFound" do
    roundtrip NotFound
  it "serializes to wire format" do
    writeJSON InvalidInput `shouldEqual` "\"invalid_input\""

messageSpec :: Spec Unit
messageSpec = describe "Message helpers" do
  it "textMessage creates a text/plain message" do
    let msg = textMessage "user" "hello"
    msg.role `shouldEqual` "user"
    (msg.parts # map _.content_type) `shouldEqual` [ "text/plain" ]
    (msg.parts # map _.content) `shouldEqual` [ Just "hello" ]
  it "userMessage sets role to user" do
    let msg = userMessage "hello"
    msg.role `shouldEqual` "user"

runSpec :: Spec Unit
runSpec = describe "Run" do
  it "roundtrips a minimal run" do
    let json = """{"run_id":"r1","status":"created"}"""
    case withStringErrors (readJSON json) of
      Right (run :: { run_id :: RunId, status :: RunStatus }) -> do
        run.run_id `shouldEqual` RunId "r1"
        run.status `shouldEqual` Created
      Left err -> fail err

acpEventSpec :: Spec Unit
acpEventSpec = describe "ACPEvent" do
  it "roundtrips RunCreated event" do
    let run = { agent_name: Nothing, run_id: RunId "r1", status: Created, output: Nothing, error: Nothing, session_id: Nothing, await_request: Nothing, created_at: Nothing, finished_at: Nothing }
    let event = RunCreated run
    let json = writeJSON event
    case withStringErrors (readJSON json) of
      Right (parsed :: ACPEvent) -> case parsed of
        RunCreated r -> r.run_id `shouldEqual` RunId "r1"
        _ -> fail "expected RunCreated event"
      Left err -> fail err
  it "roundtrips RunCompleted event" do
    let run = { agent_name: Just (AgentName "test-agent"), run_id: RunId "r2", status: Completed, output: Nothing, error: Nothing, session_id: Nothing, await_request: Nothing, created_at: Nothing, finished_at: Nothing }
    let event = RunCompleted run
    let json = writeJSON event
    case withStringErrors (readJSON json) of
      Right (parsed :: ACPEvent) -> case parsed of
        RunCompleted r -> do
          r.run_id `shouldEqual` RunId "r2"
          r.agent_name `shouldEqual` Just (AgentName "test-agent")
        _ -> fail "expected RunCompleted event"
      Left err -> fail err
  it "roundtrips ErrorEvent" do
    let err = { code: ServerError, message: "something broke", status: Just 500 }
    let event = ErrorEvent err
    let json = writeJSON event
    case withStringErrors (readJSON json) of
      Right (parsed :: ACPEvent) -> case parsed of
        ErrorEvent e -> do
          e.code `shouldEqual` ServerError
          e.message `shouldEqual` "something broke"
          e.status `shouldEqual` Just 500
        _ -> fail "expected ErrorEvent"
      Left e -> fail e

roundtrip :: forall a. Eq a => Show a => WriteForeign a => ReadForeign a => a -> Aff Unit
roundtrip value = do
  let json = writeJSON value
  case withStringErrors (readJSON json) of
    Right parsed -> parsed `shouldEqual` value
    Left err -> fail err
