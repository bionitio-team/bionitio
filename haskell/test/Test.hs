module Test where

import Test.Hspec (hspec, describe, it, shouldBe, SpecWith)
import Stats (sequenceStats, Stats(..))
import Bio.Sequence.Fasta (mkSeqs)
import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)

testSequenceStats :: String -> Integer -> String -> (Maybe Stats) -> SpecWith ()
testSequenceStats description minlen fastaContents expected =
   it description $
      sequenceStats minlen (mkSeqs [fromString s | s <- lines fastaContents])
         `shouldBe` expected

main :: IO ()
main = hspec $ do
   describe "sequenceStats" $ do
      testSequenceStats "zero byte input" 0 "" Nothing  
      testSequenceStats "single newline input" 0 "\n" Nothing  
      testSequenceStats "single greater than" 0 ">"  
         (Just (Stats {numSequences = 1, numBases = 0,
                       minSequenceLength = 0, maxSequenceLength = 0})) 
      testSequenceStats "one sequence" 0 ">header\nATGC\nA"
         (Just (Stats {numSequences = 1, numBases = 5,
                       minSequenceLength = 5, maxSequenceLength = 5}))
      testSequenceStats "two sequences" 0 ">header1\nATGC\nAGG\n>header2\nTT\n" 
         (Just (Stats {numSequences = 2, numBases = 9,
                       minSequenceLength = 2, maxSequenceLength = 7}))
      testSequenceStats "no header" 0 "no header\n" Nothing
      testSequenceStats "minlen less than all" 2 ">header1\nATGC\nAGG\n>header2\nTT\n"
         (Just (Stats {numSequences = 2, numBases = 9,
                       minSequenceLength = 2, maxSequenceLength = 7}))
      testSequenceStats "minlen greater than one" 3 ">header1\nATGC\nAGG\n>header2\nTT\n" 
         (Just (Stats {numSequences = 1, numBases = 7,
                       minSequenceLength = 7, maxSequenceLength = 7}))
      testSequenceStats "minlen greater than all" 8 ">header1\nATGC\nAGG\n>header2\nTT\n" 
         Nothing
