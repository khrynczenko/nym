module TestSearch
    ( testSearchForSynonyms
    ) where

import Test.Hspec ( describe
                  , it
                  , shouldBe
                  , shouldNotBe
                  , Arg
                  , SpecWith
                  )

import Search (searchForSynonyms, Handle)

testSearchForSynonyms :: Handle -> SpecWith (Arg Bool)
testSearchForSynonyms handle = do
    describe "Look for synonyms in database." $ do
        it "Retrieves synonyms if word exists in database." $ do
            foundSynonyms <- searchForSynonyms handle "abuse"
            length foundSynonyms `shouldNotBe` 0
        it "Returns empty list if word does not exist in database." $ do
            foundSynonyms <- searchForSynonyms handle "notexistingword"
            length foundSynonyms `shouldBe` 0
        it "Does not depend on the case of letters." $ do
            foundSynonyms <- searchForSynonyms handle "aBuSe"
            length foundSynonyms `shouldNotBe` 0
