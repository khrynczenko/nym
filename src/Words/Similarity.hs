module Words.Similarity 
    ( findMostSimilarWords
    )
where

import Data.Array
import Data.Text (Text)
import qualified Data.Text as T

similarityThreshold :: Int
similarityThreshold = 2

findMostSimilarWords :: Text -> [Text] -> [Text]
findMostSimilarWords word allWords = similarWords
  where
    similarWords = filter (areWordsSimilar word) allWords

areWordsSimilar :: Text -> Text -> Bool
areWordsSimilar w1 w2 = distance <= similarityThreshold
  where
    w1' = T.unpack w1
    w2' = T.unpack w2
    distance = computeLevensteinDistance w1' w2'

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
