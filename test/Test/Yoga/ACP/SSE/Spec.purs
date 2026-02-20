module Test.Yoga.ACP.SSE.Spec where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Exception (error) as Exn
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Yoga.ACP.SSE (SSEvent, parseSSE)
import Yoga.Om as Om
import Yoga.Om.Strom as Strom

spec :: Spec Unit
spec = describe "Yoga.ACP.SSE" do
  singleEventSpec
  multiEventSpec
  multiLineDataSpec
  commentSpec
  chunkBoundarySpec
  emptyFieldsSpec
  noEventFieldSpec

singleEventSpec :: Spec Unit
singleEventSpec = describe "single event" do
  it "parses a complete SSE event" do
    events <- collectSSE [ "event: message\ndata: hello\n\n" ]
    case events of
      [ e ] -> do
        e.event `shouldEqual` Just "message"
        e."data" `shouldEqual` "hello"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))

multiEventSpec :: Spec Unit
multiEventSpec = describe "multiple events" do
  it "parses two events in one chunk" do
    events <- collectSSE [ "event: a\ndata: first\n\nevent: b\ndata: second\n\n" ]
    case events of
      [ e1, e2 ] -> do
        e1.event `shouldEqual` Just "a"
        e1."data" `shouldEqual` "first"
        e2.event `shouldEqual` Just "b"
        e2."data" `shouldEqual` "second"
      _ -> fail ("expected 2 events, got " <> show (map _.event events))

multiLineDataSpec :: Spec Unit
multiLineDataSpec = describe "multi-line data" do
  it "joins multiple data lines with newlines" do
    events <- collectSSE [ "event: msg\ndata: line1\ndata: line2\ndata: line3\n\n" ]
    case events of
      [ e ] -> do
        e.event `shouldEqual` Just "msg"
        e."data" `shouldEqual` "line1\nline2\nline3"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))

commentSpec :: Spec Unit
commentSpec = describe "comments" do
  it "ignores comment lines starting with colon" do
    events <- collectSSE [ ": this is a comment\nevent: msg\ndata: hello\n\n" ]
    case events of
      [ e ] -> do
        e.event `shouldEqual` Just "msg"
        e."data" `shouldEqual` "hello"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))

chunkBoundarySpec :: Spec Unit
chunkBoundarySpec = describe "chunk boundaries" do
  it "handles event split across two chunks" do
    events <- collectSSE [ "event: msg\nda", "ta: hello\n\n" ]
    case events of
      [ e ] -> do
        e.event `shouldEqual` Just "msg"
        e."data" `shouldEqual` "hello"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))
  it "handles event split across three chunks" do
    events <- collectSSE [ "ev", "ent: test\ndata: val", "ue\n\n" ]
    case events of
      [ e ] -> do
        e.event `shouldEqual` Just "test"
        e."data" `shouldEqual` "value"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))
  it "handles newline split at chunk boundary" do
    events <- collectSSE [ "event: x\ndata: y\n", "\n" ]
    case events of
      [ e ] -> do
        e.event `shouldEqual` Just "x"
        e."data" `shouldEqual` "y"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))

emptyFieldsSpec :: Spec Unit
emptyFieldsSpec = describe "empty fields" do
  it "handles blank lines between events" do
    events <- collectSSE [ "event: a\ndata: first\n\n\n\nevent: b\ndata: second\n\n" ]
    case events of
      [ e1, e2 ] -> do
        e1."data" `shouldEqual` "first"
        e2."data" `shouldEqual` "second"
      _ -> fail ("expected 2 events, got " <> show (map _.event events))

noEventFieldSpec :: Spec Unit
noEventFieldSpec = describe "no event field" do
  it "emits event with Nothing for event field" do
    events <- collectSSE [ "data: hello\n\n" ]
    case events of
      [ e ] -> do
        e.event `shouldEqual` Nothing
        e."data" `shouldEqual` "hello"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))
  it "handles id field" do
    events <- collectSSE [ "id: 42\nevent: msg\ndata: hello\n\n" ]
    case events of
      [ e ] -> do
        e.id `shouldEqual` Just "42"
        e.event `shouldEqual` Just "msg"
      _ -> fail ("expected 1 event, got " <> show (map _.event events))

collectSSE :: Array String -> Aff (Array SSEvent)
collectSSE chunks = do
  let strom = parseSSE (Strom.fromArray chunks)
  result <- Om.runReader {} (Strom.runCollect strom)
  case result of
    Right events -> pure events
    Left _ -> throwError (Exn.error "SSE parse failed")
