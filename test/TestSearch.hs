module TestSearch
    ( testLookForNyms
    ) where

import Test.Hspec ( describe
                  , it
                  , shouldBe
                  , shouldNotBe
                  , Arg
                  , SpecWith
                  )

import Search (lookForNyms, Handle, NymsCategory(..))

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

-- testSearchForWord :: Handle -> SpecWith (Arg Bool)
-- testSearchForWord handle = do
--     describe "Look for a word in database." $ do
--         it "Retrieves a word if it exists in database." $ do
--             foundWord <- searchForWord handle "abuse"
--             isJust foundWord `shouldBe` True
            