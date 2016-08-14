{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main 
Description : The main entry point for the program.
Copyright   : (c) Bernie Pope, 2016 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Stability   : experimental
Portability : POSIX

This module orchestrates the overall behaviour of the program. The main
parts are:
 * Parse command line arguments. 
 * Process each FASTA file in sequence.
 * Pretty print output. 
-}

module Main where

import Stats
   (sequenceStats, Stats(..), average)
import Bio.Sequence.Fasta
   (readFasta, hReadFasta, Sequence)
import System.IO (stdin)
import Options.Applicative 
   (Parser, option, auto, long, short, metavar, help, value,
   many, argument, str, info, execParser, switch,
   fullDesc, (<*>), (<$>), (<>), helper, progDesc)
import Data.List
   (intercalate)

-- | Default value for the --minlen command line argument. Setting it to
-- zero means that, by default, no sequences will be skipped.
defaultMinLengthThreshold :: Integer
defaultMinLengthThreshold = 0

-- | Header row for the output.
header :: String
header = "FILENAME\tNUMSEQ\tTOTAL\tMIN\tAVG\tMAX"

-- | Brief description of the program for command line help/usage message.
programDescription
   = "Read FASTA file(s), compute and print basic statistics for each one"

-- | Command line argument options.
data Options = Options
   { -- | Minimum length sequence considered by the program. Sequences
     -- shorter than this length are ignored.
     minLengthThreshold :: Integer
     -- | If True, print the version of the program and exit.
   , version :: Bool
     -- | If True, make the program produce more detailed output
     -- about its progress.
   , verbose :: Bool
     -- | A possibly empty list of FASTA file paths to be processed
     -- by the program. 
   , fastaFiles :: [FilePath]
   }
   deriving (Eq, Ord, Show)

-- | Specification of the command line options for the program:
--  * --minlen N, N is a non-negative integer 
--  * --version
--  * --verbose 
--  * list of FASTA filename paths
defineOptions :: Parser Options
defineOptions = Options
   <$> option auto 
          (long "minlen"
           <> short 'm'
           <> metavar "N"
           <> help ("Minimum length sequence to include in stats (default="
                 ++ show defaultMinLengthThreshold ++ ")")
           <> value defaultMinLengthThreshold)
   <*> switch
          (long "version"
           <> short 'v'
           <> help "Print version and exit")
   <*> switch
          (long "verbose"
           <> help "Print more stuff about what's happening")
   -- Allow zero of more FASTA files to be specified as positional arguments.
   <*> many (argument str (metavar "[FASTA_FILE ...]"))

-- | Compute statistics for each input FASTA file specified on the
-- command line. If no files were specified then process the
-- stdin as a single FASTA file. 
processFastaFiles :: Options    -- ^ Command line options
                  -> [FilePath] -- ^ Possibly empty list of FASTA file paths
                  -> IO ()
-- No files specified on command line, so read from stdin
processFastaFiles options [] =
   hReadFasta stdin >>= processFile options "stdin"
-- One or more files specified on command line, process
-- each one in sequence.
processFastaFiles options files@(_:_) =
   mapM_ (\file -> readFasta file >>= processFile options file) files

-- | Compute statistics for the contents of a single FASTA file
processFile :: Options    -- ^ Command line options
            -> String     -- ^ Label for the output (typically filename)
            -> [Sequence] -- ^ Contents of FASTA file
            -> IO ()
processFile options label sequences =
   putStrLn $ prettyOutput label $
      sequenceStats (minLengthThreshold options) sequences 

-- | Render program outout as a String. If no statistics are available it means
-- that the input file was empty, so no min, max or average is available. In
-- that case we just show zeros for num sequences and num bases, and dashes for the
-- remaining values.
prettyOutput :: String      -- ^ A label for the input file. Normally the file name.
             -> Maybe Stats -- ^ Computed statistics about the file.
                            -- None if file was empty.
             -> String      -- ^ Rendered output, tab separated.
prettyOutput label Nothing =
   label ++ "\t0\t0\t-\t-\t-"
prettyOutput label (Just stats@Stats {..}) =
   intercalate "\t" (label : numbers)
   where
   -- If we cannot compute an average, show a dash.
   averageStr = maybe "-" show (average stats)
   numbers = [ show numSequences, show numBases, show minSequenceLength
             , averageStr, show maxSequenceLength ]

-- | The entry point for the program.
--  * Parse the command line arguments.
--  * Print the output header.
--  * Process each input FASTA file, compute stats and display
--    results.
main :: IO ()
main = do
   options <- execParser optionParser 
   putStrLn header
   processFastaFiles options $ fastaFiles options
   where
   optionParser =
      info (helper <*> defineOptions)
           (fullDesc <> progDesc programDescription)
