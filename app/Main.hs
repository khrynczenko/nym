module Main where

import Options.Applicative ((<**>))
import qualified Options.Applicative as OP
import qualified Data.Text.IO as TIO

import Data.Text (Text)
import Cli (argumentsParser, Arguments(..), word)
import Search (searchForSynonyms)


import Search as S

data NymState = NymState 
    { dbHandle :: S.Handle
    , arguments :: Arguments
    }

dbFilename :: Text
dbFilename = "nyms.db"

createNymState :: S.DatabasePath -> Arguments -> IO NymState
createNymState dbPath args = do
    handle <- S.createHandle dbPath
    return (NymState handle args)

main :: IO ()
main = do
    args <- OP.execParser opts
    state <- createNymState dbFilename args
    run state
    where
        opts = OP.info
            (argumentsParser <**> OP.helper)
            (OP.fullDesc <> OP.progDesc "Synonym finder.")

run :: NymState -> IO ()
run state = do
    synonyms <- searchForSynonyms db w
    let firstN = take toTake synonyms
    mapM_ TIO.putStrLn firstN
  where
    w = word $ arguments state
    toTake = nResults $ arguments state
    db = dbHandle state