module Cli 
    ( Arguments(Arguments)
    , argumentsParser
    , getWord
    , getNResults
    , getCategory
    ) where

import Data.Text (Text)
import Options.Applicative (Parser)
import qualified Options.Applicative as OP

import Words (Category(Synonyms, Antonyms))

data Arguments = Arguments { getWord :: Text
                           , getNResults :: Int
                           , getCategory :: Category
                           }

argumentsParser :: Parser Arguments
argumentsParser =
    Arguments 
    <$> OP.strArgument
        ( OP.metavar "WORD" 
        <> OP.help "Word for which nyms will be searched.")
    <*> OP.option OP.auto 
        ( OP.short 'n' 
        <> OP.value 5 
        <> OP.showDefault 
        <> OP.help "How many nyms to search for.")
    <*> OP.flag Synonyms Antonyms
        ( OP.short 'a' 
        <> OP.help "Look for antonyms instead of synonyms.")
