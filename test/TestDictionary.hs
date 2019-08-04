module TestDictionary
    ( testLookForNyms
    , testGetAllWords
    ) where

import Test.Hspec ( describe
                  , it
                  , shouldBe
                  , shouldNotBe
                  , Arg
                  , SpecWith
                  )

import Dictionary ( lookForNyms
                  , getAllWords
                  , Handle
                  , NymsCategory(..))

testLookForNyms :: Handle -> SpecWith (Arg Bool)
testLookForNyms handle = do
    describe "Look for synonyms in database." $ do
        it "Retrieves synonyms if word exists in database." $ do
            foundSynonyms <- lookForNyms handle Synonyms "abuse"
            length foundSynonyms `shouldNotBe` 0
        it "Returns empty list if word does not exist in database." $ do
            foundSynonyms <- lookForNyms handle Synonyms "notexistingword"
            length foundSynonyms `shouldBe` 0
        it "Does not depend on the case of letters." $ do
            foundSynonyms <- lookForNyms handle Synonyms "aBuSe"
            length foundSynonyms `shouldNotBe` 0
    describe "Look for antonyms in database." $ do
        it "Retrieves antonyms if word exists in database." $ do
            foundSynonyms <- lookForNyms handle Antonyms "abuse"
            length foundSynonyms `shouldNotBe` 0
        it "Returns empty list if word does not exist in database." $ do
            foundSynonyms <- lookForNyms handle Antonyms "notexistingword"
            length foundSynonyms `shouldBe` 0
        it "Does not depend on the case of letters." $ do
            foundSynonyms <- lookForNyms handle Antonyms "aBuSe"
            length foundSynonyms `shouldNotBe` 0

testGetAllWords :: Handle -> SpecWith (Arg Bool)
testGetAllWords handle = do
    describe "Get all words from databse." $ do
        it "Retrieves all words from database." $ do
            foundWords <- getAllWords handle
            length foundWords `shouldBe` 5854
            