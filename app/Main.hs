module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative ((<**>))
import qualified Options.Applicative as OP

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
        (OP.fullDesc <> OP.progDesc description)
    description = mconcat ["nym - synonyms lookup tool"]

run :: NymState -> IO ()
run state = do
    synonyms <- searchForSynonyms db w
    let firstN = take toTake synonyms
    mapM_ TIO.putStrLn firstN
  where
    w = word $ arguments state
    toTake = nResults $ arguments state
    db = dbHandle state