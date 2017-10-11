module Main where

{-|
Module      : Main 
Description : Unit tests for bionitio-hs 
Copyright   : (c) Bernie Pope, 2016 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Stability   : experimental
Portability : POSIX

Intended to be used in conjunction with stack test.
-}

import Test.Hspec (hspec, describe, it, shouldBe, SpecWith)
import Bio.Sequence.Fasta (mkSeqs)
import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)
import Stats (sequenceStats, Stats(..))

-- | Helper function to check if the actual output of 'sequenceStats' is equal
-- to the expected output.
testSequenceStats :: String        -- ^ Textual description of the test case.
                  -> Integer       -- ^ Minimum sequence length.
                  -> String        -- ^ Contents of FASTA file.
                  -> (Maybe Stats) -- ^ Excpected output
                  -> SpecWith ()
testSequenceStats description minlen fastaContents expected =
   it description $
      sequenceStats minlen (mkSeqs [fromString s | s <- lines fastaContents])
         `shouldBe` expected

main :: IO ()
main = hspec $ do
   describe "sequenceStats" $ do
      -- Completely empty input file.
      testSequenceStats "zero byte input" 0 "" Nothing  
      -- Input file is just an single newline character.
      testSequenceStats "single newline input" 0 "\n" Nothing  
      -- Input file is just a single greater than character.
      testSequenceStats "single greater than" 0 ">"  
         (Just (Stats {numSequences = 1, numBases = 0,
                       minSequenceLength = 0, maxSequenceLength = 0})) 
      -- Input file contains a single sequence, split over two lines.
      testSequenceStats "one sequence" 0 ">header\nATGC\nA"
         (Just (Stats {numSequences = 1, numBases = 5,
                       minSequenceLength = 5, maxSequenceLength = 5}))
      -- Input file contains two sequences.
      testSequenceStats "two sequences" 0 ">header1\nATGC\nAGG\n>header2\nTT\n" 
         (Just (Stats {numSequences = 2, numBases = 9,
                       minSequenceLength = 2, maxSequenceLength = 7}))
      -- Input file contains a sequence without a corresponding header.
      testSequenceStats "no header" 0 "no header\n" Nothing
      -- Minlen is less than all (2) sequences in the input file.
      testSequenceStats "minlen less than all" 2 ">header1\nATGC\nAGG\n>header2\nTT\n"
         (Just (Stats {numSequences = 2, numBases = 9,
                       minSequenceLength = 2, maxSequenceLength = 7}))
      -- Minlen is greater than one (out of 2) sequences in the input file.
      testSequenceStats "minlen greater than one" 3 ">header1\nATGC\nAGG\n>header2\nTT\n" 
         (Just (Stats {numSequences = 1, numBases = 7,
                       minSequenceLength = 7, maxSequenceLength = 7}))
      -- Minlen is greater than all (2) sequences in input file
      testSequenceStats "minlen greater than all" 8 ">header1\nATGC\nAGG\n>header2\nTT\n" 
         Nothing
