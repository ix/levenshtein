module Main where

import Test.Hspec
import Data.Vector.Levenshtein
import qualified Data.Vector as V

main :: IO ()
main = hspec $ do
  describe "Data.Vector.Levenshtein" $ do
    describe "distance" $ do
      it "returns a known good value for a distance check" $ do
        distance "distance" "dancing" `shouldBe` 6
    describe "distance'" $ do
      it "returns a correct matrix for a known distance check" $ do
        let matrix = distance' "sitting" "kitten"
        let known  = V.fromList $ map V.fromList [ [0, 1, 2, 3, 4, 5, 6]
                                                 , [1, 1, 2, 3, 4, 5, 6]
                                                 , [2, 2, 1, 2, 3, 4, 5]
                                                 , [3, 3, 2, 1, 2, 3, 4]
                                                 , [4, 4, 3, 2, 1, 2, 3]
                                                 , [5, 5, 4, 3, 2, 2, 3]
                                                 , [6, 6, 5, 4, 3, 3, 2]
                                                 , [7, 7, 6, 5, 4, 4, 3] ]
        matrix `shouldBe` known
