module WordDistance 
    ( findMostSimilarWords
    )
where

import Data.Array
import Data.Text (Text)
import qualified Data.Text as T

findMostSimilarWords :: Text -> [Text] -> [Text]
findMostSimilarWords word allWords = withoutWordThatIsTheSame mostSimilarWords
  where
    mostSimilarWords = map fst $ filter (\x -> snd x == bestCost) wordsAndCosts
    withoutWordThatIsTheSame = filter (\x -> x /= word)
    distances = map (computeDistance . T.unpack) allWords
    computeDistance = computeLevensteinDistance (T.unpack word)
    -- bestCost gets 0 if distance is 4 so the list will be empty
    -- distance of 4 is probably completly different word
    bestCost = if minimum distances < 4 then minimum distances else 0
    wordsAndCosts = zip allWords distances

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
