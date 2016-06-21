{-# LANGUAGE RecordWildCards #-}
module Main where

import Stats (sequenceStats, Stats(..), average)
import Bio.Sequence.Fasta
   ( readFasta, hReadFasta, seqlength, Sequence )
import System.IO ( stdin )
import Options.Applicative 
   ( Parser, option, auto, long, metavar, help, value
   , many, argument, str, info, execParser, switch
   , fullDesc, (<*>), (<$>), (<>), helper, progDesc )
import Data.List
   ( foldl', intersperse )

defaultMinLengthThreshold :: Integer
defaultMinLengthThreshold = 0

data Options = Options
   { minLengthThreshold :: Integer
   , version :: Bool
   , verbose :: Bool
   , fastaFiles :: [FilePath]
   }
   deriving (Eq, Ord, Show)

defineOptions :: Parser Options
defineOptions = Options
   <$> option auto 
         (long "minlen"
            <> metavar "N"
            <> help ("Minimum length sequence to include in stats (default=" ++ show defaultMinLengthThreshold ++ ")")
            <> value defaultMinLengthThreshold)
   <*> switch (long "version" <> help "Print version and exit")
   <*> switch (long "verbose" <> help "Print more stuff about what's happening")
   <*> many (argument str (metavar "[FASTA_FILE ...]"))

main :: IO ()
main = do
   options <- execParser optionParser 
   processFastaFiles options $ fastaFiles options
   where
   optionParser =
      info (helper <*> defineOptions)
           (fullDesc <> progDesc "Print fasta stats")

header :: String
header = "FILENAME\tNUMSEQ\tTOTAL\tMIN\tAVG\tMAX"

processFastaFiles :: Options -> [FilePath] -> IO ()
processFastaFiles options [] = do
   sequences <- hReadFasta stdin
   putStrLn header
   putStrLn $ prettyOutput "stdin" $ 
      sequenceStats (minLengthThreshold options) sequences 
processFastaFiles options files@(_:_) =
   putStrLn header >> mapM_ (processFile options) files

processFile :: Options -> FilePath -> IO ()
processFile options filePath = do
   sequences <- readFasta filePath
   putStrLn $ prettyOutput filePath $
      sequenceStats (minLengthThreshold options) sequences 

prettyOutput :: FilePath -> Maybe Stats -> String
prettyOutput filePath Nothing =
   filePath ++ "\t0\t0\t-\t-\t-"
prettyOutput filePath (Just stats@(Stats {..})) =
   concat $ intersperse "\t" (filePath : numbers)
   where
   averageStr = maybe "-" show (average stats)
   numbers = [ show numSequences, show numBases, show minSequenceLength
             , averageStr, show maxSequenceLength ]
