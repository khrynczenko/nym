module Main where

import Options.Applicative ((<**>))
import qualified Options.Applicative as OP
import qualified Data.Text.IO as TIO
import Cli (argumentsParser, Arguments(..), word)
import Nym (NymState, dbFilename, createNymState, arguments, dbHandle)
import Search (searchForSynonyms)

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