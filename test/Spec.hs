import Test.Hspec (hspec)

import Words.Database (createHandle)
import Words.TestDatabase
import Words.TestSimilarity

main :: IO ()
main = do
    handle <- createHandle "nyms.db"
    hspec $ do
        testLookForNyms handle
        testFindMostSimilarWords
