module Words.TestSimilarity
    ( testFindMostSimilarWords
    ) where

import Test.Hspec ( describe
                  , it
                  , shouldBe
                  , Arg
                  , SpecWith
                  )

import Words.Similarity (findMostSimilarWords)

testFindMostSimilarWords :: SpecWith (Arg Bool)
testFindMostSimilarWords = do
    describe "Finding words that are similar." $ do
        it "Finds most similar words." $ do
            findMostSimilarWords "wors" ["word", "work", "dork", "XD", "bit"] 
            `shouldBe` ["word", "work"]