module Words.TestDatabase
    ( testRetrieveNyms
    , testRetrieveWords
    ) where

import Test.Hspec ( describe
                  , it
                  , shouldBe
                  , shouldNotBe
                  , Arg
                  , SpecWith
                  )

import Words (Category(Synonyms, Antonyms))                
import Words.Database ( retrieveNyms
                      , retrieveWords
                      , Handle
                      )

testRetrieveNyms :: Handle -> SpecWith (Arg Bool)
testRetrieveNyms handle = do
    describe "Look for synonyms in database." $ do
        it "Retrieves synonyms if word exists in database." $ do
            foundSynonyms <- retrieveNyms handle Synonyms "abuse"
            length foundSynonyms `shouldNotBe` 0
        it "Returns empty list if word does not exist in database." $ do
            foundSynonyms <- retrieveNyms handle Synonyms "notexistingword"
            length foundSynonyms `shouldBe` 0
        it "Does not depend on the case of letters." $ do
            foundSynonyms <- retrieveNyms handle Synonyms "aBuSe"
            length foundSynonyms `shouldNotBe` 0
    describe "Look for antonyms in database." $ do
        it "Retrieves antonyms if word exists in database." $ do
            foundSynonyms <- retrieveNyms handle Antonyms "abuse"
            length foundSynonyms `shouldNotBe` 0
        it "Returns empty list if word does not exist in database." $ do
            foundSynonyms <- retrieveNyms handle Antonyms "notexistingword"
            length foundSynonyms `shouldBe` 0
        it "Does not depend on the case of letters." $ do
            foundSynonyms <- retrieveNyms handle Antonyms "aBuSe"
            length foundSynonyms `shouldNotBe` 0

testRetrieveWords :: Handle -> SpecWith (Arg Bool)
testRetrieveWords handle = do
    describe "Get all words from databse." $ do
        it "Retrieves all words from database." $ do
            foundWords <- retrieveWords handle
            length foundWords `shouldBe` 5854
            