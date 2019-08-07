module Words.Database 
    ( Handle()
    , DatabasePath
    , createHandle
    , retrieveNyms
    , retrieveWords
    ) where

import Data.Maybe (listToMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only(..))
import qualified Database.SQLite.Simple as DB

import Words (Category)

type DatabasePath = Text

data Handle = Handle { connection :: Connection }

createHandle :: DatabasePath -> IO Handle
createHandle dbPath = Handle <$> DB.open (T.unpack dbPath)

retrieveNyms :: Handle -> Category -> Text -> IO [Text]
retrieveNyms handle category word = do 
    maybeWordId <- searchForWordId handle lowerCaseWord
    case maybeWordId of
        (Just wordId) -> searchForNyms handle category wordId
        Nothing -> return []
  where
    lowerCaseWord = T.toLower word

retrieveWords :: Handle -> IO [Text]
retrieveWords handle = do
    allWords <- (DB.query_ conn sqlQuery) :: IO [Only Text]
    return $ fmap DB.fromOnly allWords
  where
    conn = connection handle
    sqlQuery = "SELECT words.word FROM words"

searchForNyms :: Handle -> Category -> Int -> IO [Text]
searchForNyms handle category wordId = do 
    foundNyms <- (DB.query conn getNyms (Only wordId)) :: IO [Only Text]
    return $ fmap DB.fromOnly foundNyms
  where
    conn = connection handle
    tableName = T.toLower $ ( T.pack $ show category)
    columnName = (T.toLower . T.init $ ( T.pack $ show category)) <> "_id"
    getNyms = DB.Query $ mconcat 
      [ "SELECT words.word FROM words WHERE words.id IN ( "
      , "SELECT "
      , columnName
      , " FROM "
      , tableName
      , " INNER JOIN words ON words.id=word_id "
      , "WHERE words.id=?)"
      ]

searchForWordId :: Handle -> Text -> IO (Maybe Int)
searchForWordId handle word = do 
    foundIds <- 
      (DB.query conn lookForWordId (Only (word'))) :: IO [Only Int] 
    let foundIds' = fmap DB.fromOnly foundIds
    return $ listToMaybe foundIds'
  where
    conn = connection handle
    word' = T.toLower word
    lookForWordId = DB.Query "SELECT id from words where word=?"