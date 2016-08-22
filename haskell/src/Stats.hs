{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Stats 
Description : Compute various simple statistics on a FASTA file. 
Copyright   : (c) Bernie Pope, 2016 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Stability   : experimental
Portability : POSIX

Read a single FASTA file as input and compute:
 * Num sequences.
 * Total number of bases in all sequences.
 * Minimum sequence length.
 * Maximum sequence length.
 * Average sequence length, as an integer , rounded towards zero.

Sequences whose length is less than a specified minimum length are
ignored (skipped) and not included in the calculation of the statistics.

The basic statistics cannot be computed for empty files, and those
with no reads that meet the mimimum length requirement. In such cases
the result is Nothing.

We do not make any assumptions about the type of data stored in the
FASTA file, it could be DNA, proteins or anything else. No checks are
made to ensure that the FASTA file makes sense biologically.

Whitespace within the sequences will be ignored.
-}

module Stats (sequenceStats, Stats(..), average) where

import Bio.Sequence.Fasta
   (seqlength, Sequence)
import Data.List
   (foldl')

-- | Basic statistics computed for a FASTA file. Note: average is
-- not included, but can be computed from the 'average' function.
-- Note that all values are with respect to only those reads whose
-- length is at least as long as the minimum.
data Stats =
   Stats 
   { -- | Total number of sequences in the file. 
     numSequences :: !Integer      
     -- | Total number of bases from all the sequences in the file. 
   , numBases :: !Integer
     -- | Minimum length of all sequences in the file.
   , minSequenceLength :: !Integer
     -- | Maximum length of all sequences in the file.
   , maxSequenceLength :: !Integer
   }
   deriving (Eq, Ord, Show)

-- | Average length of all sequences in a FASTA file.
-- Returns Nothing if the number of counted sequences is zero.
-- Average is rounded to an integer towards zero.
average :: Stats -> Maybe Integer
average Stats{..}
   | numSequences > 0 =
        Just $ floor (fromIntegral numBases / fromIntegral numSequences)
   | otherwise = Nothing

-- | Compute basic statistics for a list of sequences taken from
-- a FASTA file. Sequences whose length is less than the specified minimum
-- are ignored.
sequenceStats :: Integer     -- ^ Minimum sequence length. Sequences shorter
                             -- than this are ignored (skipped).
              -> [Sequence]  -- ^ A list of all the sequences from a FASTA file.
              -> Maybe Stats -- ^ Basic statistics for all the sequences. Is Nothing
                             -- if the list of sequences does not contain any
                             -- elements which are at least as long as the minimum.
sequenceStats minLength sequences =
   case filteredLengths of
      [] -> Nothing
      first:rest -> 
        Just $ foldl' updateStats (initStats first) rest 
   where
   -- Collect all sequences whose length is at least as long as the
   -- minumum. Lazy evaluation means that this can be done in a streaming
   -- fashion, so we don't have to read the entire file at once.
   filteredLengths =
      filter (\x -> sequenceLengthInteger x >= minLength) sequences

-- | Initial value for statistics, given the first sequence.
initStats :: Sequence -> Stats    
initStats sequence = Stats
   { numSequences = 1
   , numBases = thisLength 
   , minSequenceLength = thisLength 
   , maxSequenceLength = thisLength 
   }
   where
   thisLength = sequenceLengthInteger sequence

-- | Update the stats given the next sequence from the FASTA file.
updateStats :: Stats    -- ^ Current value of stats.
            -> Sequence -- ^ Next sequence from the FASTA file.
            -> Stats    -- ^ New, updated stats.
updateStats oldStats@Stats{..} sequence =
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
