{-# LANGUAGE RecordWildCards #-}
module Stats (sequenceStats, Stats(..)) where

import Bio.Sequence.Fasta
   ( seqlength, Sequence )
import Data.List
   ( foldl', intersperse )

data Stats =
   Stats 
   { numSequences :: !Integer
   , numBases :: !Integer
   , minSequenceLength :: !Integer
   , maxSequenceLength :: !Integer
   }
   deriving (Eq, Ord, Show)

sequenceStats :: Integer -> [Sequence] -> Maybe Stats
sequenceStats minLength sequences =
   case filteredLengths of
      [] -> Nothing
      first:rest -> 
        Just $ foldl' updateStats (initStats first) rest 
   where
   filteredLengths =
      filter (\x -> sequenceLengthInteger x >= minLength) sequences

initStats :: Sequence -> Stats
initStats sequence = Stats
   { numSequences = 1
   , numBases = thisLength 
   , minSequenceLength = thisLength 
   , maxSequenceLength = thisLength 
   }
   where
   thisLength = sequenceLengthInteger sequence

updateStats :: Stats -> Sequence -> Stats
updateStats oldStats@(Stats {..}) sequence =
   Stats newNumSequences newNumBases
         newMinSequenceLength newMaxSequenceLength 
   where
   newNumSequences = numSequences + 1
   thisLength = sequenceLengthInteger sequence
   newNumBases = numBases + thisLength 
   newMinSequenceLength
      = min thisLength minSequenceLength
   newMaxSequenceLength
      = max thisLength maxSequenceLength 

sequenceLengthInteger :: Sequence -> Integer
sequenceLengthInteger = fromIntegral . seqlength
