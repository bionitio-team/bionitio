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
import Options.Applicative 
   (Parser, ParserInfo (infoFailureCode), option, auto, long, short,
   metavar, help, value, many, argument, str, info, execParser, switch,
   fullDesc, (<*>), (<$>), (<>), helper, progDesc)
import Data.List
   (intercalate)
import Control.Exception
   (catch, IOException)
import System.Environment
   (getProgName)
import System.Exit
   (exitWith, ExitCode(..))
import System.IO
   (stdin, hPutStrLn, stderr)

-- | File I/O error. This can occur if at least one of the input FASTA
-- files cannot be opened for reading. This can occur because the file
-- does not exist at the specified path, or biotool does not have permission
-- to read from the file.  
exitFileError = 1
-- | A command line error occurred. This can happen if the user specifies
-- an incorrect command line argument. In this circumstance biotool will
-- also print a usage message to the standard error device (stderr).  
exitCommandLineError = 2

-- | Exit the program, printing an error message on stderr, and returning
-- a specific error code. The program name is prefixed onto the front of 
-- the error message.
exitWithError :: String -- ^ Reason for the error. 
              -> Int    -- ^ The exit code to return from the program.
              -> IO ()
exitWithError message exitCode = do
   programName <- getProgName
   let errorStr = programName ++ " ERROR: " ++ message
   hPutStrLn stderr errorStr
   exitWith (ExitFailure exitCode)

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
   mapM_ tryProcessFile files
   where
   -- Catch any IO excpetions raised in the processing of the file
   tryProcessFile :: FilePath -> IO ()
   tryProcessFile file =
      catch
         (readFasta file >>= processFile options file)
         (\exception ->
            exitWithError (show (exception :: IOException))
               exitFileError)

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

-- | Command line parser.
-- We require that the program exits with status 2
-- if the command line argument parsing fails.
optionParser :: ParserInfo Options
optionParser = thisParserInfo { infoFailureCode = exitCommandLineError }
   where
   thisParserInfo =
      info (helper <*> defineOptions)
         (fullDesc <> progDesc programDescription)

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
