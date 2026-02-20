module Yoga.ACP.Store (initStore, storeInputMessages, storeMessage, tapStore) where

import Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Data.String as String
import Yoga.ACP.Types (ACPEvent(..), Message)
import Yoga.Om (Om)
import Yoga.Om as Om
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom
import Yoga.SQLite.SQLite (Connection, SQL(..), execute, executeSimple, toSQLiteValue)

initStore :: forall ctx err. Connection -> Om ctx err Unit
initStore conn = void $ executeSimple ddl conn # Om.toOm
  where
  ddl = SQL """
    CREATE TABLE IF NOT EXISTS messages (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id TEXT NOT NULL,
      role TEXT NOT NULL,
      content TEXT,
      created_at TEXT DEFAULT (datetime('now'))
    );
    CREATE INDEX IF NOT EXISTS idx_messages_run_id ON messages (run_id);
  """

storeMessage :: forall ctx err. Connection -> String -> Message -> Om ctx err Unit
storeMessage conn runId msg = void $ execute sql params conn # Om.toOm
  where
  sql = SQL "INSERT INTO messages (run_id, role, content) VALUES (?, ?, ?)"
  params = [ toSQLiteValue runId, toSQLiteValue msg.role, toSQLiteValue (textContent msg) ]

storeInputMessages :: forall ctx err. Connection -> String -> Array Message -> Om ctx err Unit
storeInputMessages conn runId = traverse_ (storeMessage conn runId)

tapStore :: forall ctx err. Connection -> String -> Strom ctx err ACPEvent -> Strom ctx err ACPEvent
tapStore conn runId = Strom.tapMStrom onEvent
  where
  onEvent (MessageCompleted msg) = storeMessage conn runId msg
  onEvent _ = pure unit

textContent :: Message -> String
textContent msg = msg.parts # Array.mapMaybe _.content # String.joinWith "\n"
