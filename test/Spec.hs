import Test.Hspec (hspec)

import Search (createHandle)
import TestSearch

main :: IO ()
main = do
    handle <- createHandle "nyms.db"
    hspec $ testSearchForSynonyms handle
