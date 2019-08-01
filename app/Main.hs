module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative ((<**>))
import qualified Options.Applicative as OP

import Cli ( argumentsParser
           , word
           , nymsCategory
           , Arguments(..)
           )
import Search (lookForNyms)


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
    description = mconcat ["nym - synonyms/antonyms lookup tool"]

run :: NymState -> IO ()
run state = do
    nyms <- lookForNyms db whatNymsToLookFor w
    let firstN = take toTake nyms
    mapM_ TIO.putStrLn firstN
  where
    whatNymsToLookFor = nymsCategory $ arguments state
    w = word $ arguments state
    toTake = nResults $ arguments state
    db = dbHandle state