module Main where

import qualified Options.Applicative as OP
import qualified Data.List as List

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Cli ( argumentsParser
           , getWord
           , getCategory
           , Arguments(..)
           )
import qualified Messages
import Words.Database as WDB
import Words.Similarity (findMostSimilarWords)

data ApplicationState = ApplicationState
    { getDbHandle :: WDB.Handle
    , getArguments :: Arguments
    }

dbFilename :: Text
dbFilename = "nyms.db"

createNymState :: WDB.DatabasePath -> Arguments -> IO ApplicationState
createNymState dbPath args = do
    handle <- WDB.createHandle dbPath
    return (ApplicationState handle args)

main :: IO ()
main = do
    args <- OP.execParser opts
    state <- createNymState dbFilename args
    run state
  where
    opts = OP.info
        (OP.helper <*> argumentsParser)
        (OP.fullDesc <> OP.progDesc description)
    description = mconcat ["nym - synonyms/antonyms lookup tool"]

run :: ApplicationState -> IO ()
run state = do
    nyms <- retrieveNyms dbHandle category word
    case nyms of
        [] -> do
            allWords <- retrieveWords dbHandle
            let withoutWordItself = List.delete word allWords
            let similarWords = findMostSimilarWords word withoutWordItself
            TIO.putStrLn $ Messages.buildNotFoundNyms category word similarWords
        _nyms -> do
            let firstN = take toTake _nyms
            mapM_ TIO.putStrLn firstN
  where
    category = getCategory $ getArguments state
    word = getWord $ getArguments state
    toTake = getNResults $ getArguments state
    dbHandle = getDbHandle state
