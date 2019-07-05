module Main where

import Cli (argumentsParser, Arguments(..))
import Options.Applicative
import qualified Data.Text.IO as TIO



main :: IO ()
main = run =<< execParser opts
    where
        opts = info
            (argumentsParser <**> helper)
            (fullDesc <> progDesc "Synonym finder.")

run :: Arguments -> IO ()
run (Arguments x y) = TIO.putStrLn x