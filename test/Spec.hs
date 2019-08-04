import Test.Hspec (hspec)

import Dictionary (createHandle)
import TestDictionary
import TestWordDistance

main :: IO ()
main = do
    handle <- createHandle "nyms.db"
    hspec $ do
        testLookForNyms handle
        testFindMostSimilarWords
