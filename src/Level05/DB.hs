{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM (AppM))
import Level05.DB.Types (DBComment)
import Control.Arrow

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB fn ioA =
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
  let
    dbErrorOrA = Sql.runDBAction ioA
    errorOrA = fmap (left DBError) dbErrorOrA
    errorOrB = fmap (>>= fn) errorOrA
  in AppM errorOrB
  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments (FirstAppDB conn) topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    dbComments = Sql.query conn sql (Sql.Only (getTopic topic :: Text)) :: IO [DBComment]
  in runDB (traverse fromDBComment) dbComments

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic (FirstAppDB conn) topic commentText =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    time = "2012-08-20 20:19:58 UTC"
    insertQuery = Sql.execute conn sql [getTopic topic :: Text, getCommentText commentText :: Text, time :: Text]
    result = runDB Right insertQuery
  in result

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics (FirstAppDB conn) =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    mkTopicsFromDbString :: [Sql.Only Text] -> Either Error [Topic]
    mkTopicsFromDbString = traverse (\(Sql.Only t) -> mkTopic t)

    topicStrings = Sql.query_ conn sql :: IO [Sql.Only Text]
    result = runDB mkTopicsFromDbString topicStrings
  in result

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic (FirstAppDB conn) topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    resultIO = Sql.execute conn sql (Sql.Only (getTopic topic :: Text))
    result = runDB Right resultIO
  in
    result

-- Go to 'src/Level05/Core.hs' next.
