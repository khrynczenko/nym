module Cli where

import Options.Applicative (Parser)
import qualified Options.Applicative as OP
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

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
