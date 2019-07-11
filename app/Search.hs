module Search 
    ( Handle()
    , DatabasePath
    , createHandle
    , searchForSynonyms
    ) where

import qualified Data.Text as T
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only(..))
import qualified Database.SQLite.Simple as DB

type DatabasePath = Text

data Handle = Handle { connection :: Connection }

createHandle :: DatabasePath -> IO Handle
createHandle dbPath = Handle <$> DB.open (T.unpack dbPath)

searchForSynonyms :: Handle -> Text -> IO [Text]
searchForSynonyms handle word = do 
    foundIds <- (DB.query conn getWordId (Only (T.unpack word) )) :: IO [Only Int]
    let foundIds' = fmap DB.fromOnly foundIds
    let wordId = head foundIds'
    foundSynonymsIds <- (DB.query conn getSynonymsIds (Only wordId)) :: IO [Only Int]
    let foundSynonymsIds' = fmap DB.fromOnly foundSynonymsIds
    -- let foundSynonyms = fmap (head . (DB.query conn getSynonymWord)) foundSynonymsIds
    allIdsAndWords <- DB.query conn getAllIdsAndWords ()  :: IO [(Int, Text)]
    let synonyms = filter (\(wid, _) -> wid `elem` foundSynonymsIds') allIdsAndWords 
    return $ fmap snd synonyms
  where
    conn = connection handle
    getWordId = DB.Query "SELECT id from words where word=?"
    getSynonymsIds = DB.Query "SELECT synonym_id from synonyms where word_id=?"
    -- getSynonymWord = DB.Query "SELECT word from words where id=?"
    getAllIdsAndWords = DB.Query "SELECT * from words"
