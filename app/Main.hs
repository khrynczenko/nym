module Main where

import qualified Options.Applicative as OP

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Cli ( argumentsParser
           , getWord
           , getCategory
           , Arguments(..)
           )
import qualified Messages as Messages
import Words.Database (lookForNyms, getAllWords)
import Words.Database as WDB
import Words.Similarity (findMostSimilarWords)

data NymState = NymState 
    { getDbHandle :: WDB.Handle
    , getArguments :: Arguments
    }

dbFilename :: Text
dbFilename = "nyms.db"

createNymState :: WDB.DatabasePath -> Arguments -> IO NymState
createNymState dbPath args = do
    handle <- WDB.createHandle dbPath
    return (NymState handle args)

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

run :: NymState -> IO ()
run state = do
    nyms <- lookForNyms dbHandle category word
    case nyms of
        [] -> do
            allWords <- getAllWords dbHandle
            let similarWords = findMostSimilarWords' allWords
            TIO.putStrLn $ Messages.buildNotFoundNyms category word similarWords
        _ -> do
            let firstN = take toTake nyms
            mapM_ TIO.putStrLn firstN
  where
    category = getCategory $ getArguments state
    word = getWord $ getArguments state
    toTake = getNResults $ getArguments state
    dbHandle = getDbHandle state
    findMostSimilarWords' = findMostSimilarWords word
