module TestWordDistance
    ( testFindMostSimilarWords
    ) where

import Test.Hspec ( describe
                  , it
                  , shouldBe
                  , Arg
                  , SpecWith
                  )

import WordDistance (findMostSimilarWords)

testFindMostSimilarWords :: SpecWith (Arg Bool)
testFindMostSimilarWords = do
    describe "Finding words that are similar" $ do
        it "Finds most similar words" $ do
            findMostSimilarWords "wors" ["word", "work", "XD", "bit"] 
            `shouldBe` ["word", "work"]