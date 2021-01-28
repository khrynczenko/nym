module Messages
    ( buildNotFoundNyms
    ) where


import Data.List (intersperse)

import Data.Text (Text)
import qualified Data.Text as T

import Words (Category)

type SimilarWords = [Text]

buildNotFoundNyms :: Category -> Text -> SimilarWords -> Text
buildNotFoundNyms category word similarWords
    | null similarWords = notFoundNymsMessage
    | otherwise = notFoundNymsMessage
                  <> maybeSimilarMessage
                  <> mconcat similarWordsMessage
  where
    category' = T.init . T.toLower . T.pack . show $ category
    notFoundNymsMessage = 
        "Could not find " <> category' <> "s" <> " for " <> word <> "." :: Text
    maybeSimilarMessage = 
        " Maybe you meant one of these:\n" 
    similarWordsMessage = intersperse "\n" similarWords
    
        
