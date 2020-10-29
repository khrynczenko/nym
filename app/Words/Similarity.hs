module Words.Similarity 
    ( findMostSimilarWords
    )
where

import Data.Array
import Data.Text (Text)
import qualified Data.Text as T

-- |Find similar words from a list of words.
findMostSimilarWords :: Text -> [Text] -> [Text]
findMostSimilarWords word allWords = bestWords
  where
    word' = T.unpack word
    allWords' = map T.unpack allWords
    scores = map (computeLevensteinDistance word') allWords'
    wordsAndScores = zip allWords scores
    bestScore = minimum scores
    bestWords = map fst $ filter (\x -> snd x == bestScore) wordsAndScores

-- |This implementation is a copied from an anwser on reddit provided by 
-- user cgibbard. All the details under 
-- https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz?utm_source=share&utm_medium=web2x
computeLevensteinDistance :: String -> String -> Int
computeLevensteinDistance xs ys = levMemo ! (n, m)
  where levMemo = 
            array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)] 
