module Search 
    ( Handle()
    , DatabasePath
    , createHandle
    , searchForSynonyms
    ) where

import Data.Maybe (listToMaybe)

import Data.Text (Text)
import qualified Data.Text as T

import Database.SQLite.Simple (Connection, Only(..))
import qualified Database.SQLite.Simple as DB

type DatabasePath = Text

data Handle = Handle { connection :: Connection }

createHandle :: DatabasePath -> IO Handle
createHandle dbPath = Handle <$> DB.open (T.unpack dbPath)

searchForSynonyms :: Handle -> Text -> IO [Text]
searchForSynonyms handle word = do 
    foundIds <- (DB.query conn getWordId (Only (T.unpack word))) :: IO [Only Int]
    let foundIds' = fmap DB.fromOnly foundIds
    let maybeWordId = listToMaybe foundIds'
    let foundSynonyms = case maybeWordId of 
            (Just wordId) -> (DB.query conn getSynonyms (Only wordId)) :: IO [Only Text]
            Nothing -> (return []) :: IO [Only Text]
    let foundSynonyms' = fmap  (fmap DB.fromOnly) foundSynonyms
    foundSynonyms'
  where
    conn = connection handle
    getWordId = DB.Query "SELECT id from words where word=?"
    getSynonyms = DB.Query $ mconcat 
      [ "SELECT words.word FROM words WHERE words.id IN ( "
      , "SELECT synonyms.synonym_id FROM synonyms "
      , "INNER JOIN words ON words.id=synonyms.word_id "
      , "WHERE words.id=?)"
      ]
