module Cli 
    ( Arguments(Arguments)
    , argumentsParser
    , word
    , nResults
    ) where

import Data.Text (Text)
import Options.Applicative (Parser)
import qualified Options.Applicative as OP

data Arguments = Arguments { word :: Text
                           , nResults :: Int
                           }

argumentsParser :: Parser Arguments
argumentsParser =
    Arguments 
    <$> OP.strArgument
        (OP.metavar "WORD" <> OP.help "Word for which nyms will be searched.")
    <*> OP.option OP.auto 
        (OP.short 'n' <> OP.value 5 <> OP.help "How many nyms to search for.")
