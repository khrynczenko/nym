module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative ((<**>))
import qualified Options.Applicative as OP

import Cli ( argumentsParser
           , word
           , nymsCategory
           , Arguments(..)
           )
import Dictionary (lookForNyms, getAllWords)
import Dictionary as D
import WordDistance (findMostSimilarWords)

data NymState = NymState 
    { dbHandle :: D.Handle
    , arguments :: Arguments
    }

dbFilename :: Text
dbFilename = "nyms.db"

createNymState :: D.DatabasePath -> Arguments -> IO NymState
createNymState dbPath args = do
    handle <- D.createHandle dbPath
    return (NymState handle args)

main :: IO ()
main = do
    args <- OP.execParser opts
    state <- createNymState dbFilename args
    run state
  where
    opts = OP.info
        (argumentsParser <**> OP.helper)
        (OP.fullDesc <> OP.progDesc description)
    description = mconcat ["nym - synonyms/antonyms lookup tool"]

run :: NymState -> IO ()
run state = do
    nyms <- lookForNyms db whatNymsToLookFor w
    case nyms of
        [] -> do
            allWords <- getAllWords db
            let similar = similarWords allWords
            printSimilarWords whatNymsToLookFor w similar
        _ -> do
            let firstN = take toTake nyms
            mapM_ TIO.putStrLn firstN
  where
    whatNymsToLookFor = nymsCategory $ arguments state
    w = word $ arguments state
    toTake = nResults $ arguments state
    db = dbHandle state
    similarWords = findMostSimilarWords w

printSimilarWords :: NymsCategory -> Text -> [Text] -> IO ()
printSimilarWords category w similarWords = do
    if null similarWords
    then 
        TIO.putStrLn notFoundSynonymsMessage
    else
        mapM_ TIO.putStrLn 
            (notFoundSynonymsMessageButFoundSimilar : similarWords)
  where
    whichNyms = (T.toLower . T.pack . show) category
    notFoundSynonymsMessage = mconcat $
        [ "Could not find "
        , whichNyms
        , " for "
        , w
        , ". "
        ]
    notFoundSynonymsMessageButFoundSimilar =
        notFoundSynonymsMessage <> "Maybe you meant one of these:"

