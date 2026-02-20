module Yoga.ACP.SSE
  ( SSEvent
  , parseSSE
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple.Nested (type (/\), (/\))
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom

type SSEvent =
  { event :: Maybe String
  , data :: String
  , id :: Maybe String
  }

type ParserState =
  { event :: Maybe String
  , dataLines :: Array String
  , id :: Maybe String
  , buffer :: String
  }

emptyState :: ParserState
emptyState =
  { event: Nothing
  , dataLines: []
  , id: Nothing
  , buffer: ""
  }

parseSSE :: forall ctx err. Strom ctx err String -> Strom ctx err SSEvent
parseSSE = Strom.collectStrom extractEvent <<< Strom.mapAccumStrom processLine emptyState <<< splitLines
  where
  extractEvent :: Maybe SSEvent -> Maybe SSEvent
  extractEvent = identity

splitLines :: forall ctx err. Strom ctx err String -> Strom ctx err String
splitLines = Strom.bindStrom Strom.fromArray <<< Strom.mapAccumStrom splitChunk ""
  where
  splitChunk leftover chunk = do
    let combined = leftover <> chunk
    let parts = String.split (String.Pattern "\n") combined
    case Array.unsnoc parts of
      Nothing -> "" /\ []
      Just { init, last } -> last /\ init

processLine :: ParserState -> String -> ParserState /\ Maybe SSEvent
processLine state line
  | line == "" = emitEvent state
  | SCU.charAt 0 line == Just ':' = state /\ Nothing
  | otherwise = parsedState /\ Nothing
  where
  parsedState = parseField state line

emitEvent :: ParserState -> ParserState /\ Maybe SSEvent
emitEvent state
  | Array.null state.dataLines = emptyState /\ Nothing
  | otherwise = do
      let event =
            { event: state.event
            , data: String.joinWith "\n" state.dataLines
            , id: state.id
            }
      emptyState /\ Just event

parseField :: ParserState -> String -> ParserState
parseField state line = case String.indexOf (String.Pattern ":") line of
  Nothing -> applyField state line ""
  Just idx -> do
    let field = SCU.take idx line
    let rest = SCU.drop (idx + 1) line
    let value = stripLeadingSpace rest
    applyField state field value

stripLeadingSpace :: String -> String
stripLeadingSpace s = case SCU.charAt 0 s of
  Just ' ' -> SCU.drop 1 s
  _ -> s

applyField :: ParserState -> String -> String -> ParserState
applyField state field value = case field of
  "event" -> state { event = Just value }
  "data" -> state { dataLines = Array.snoc state.dataLines value }
  "id" -> state { id = Just value }
  _ -> state
