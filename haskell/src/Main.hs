{-# LANGUAGE RecordWildCards #-}
module Main where

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
   putStrLn $ prettyOutput "stdin" $ sequenceStats options sequences 
processFastaFiles options files@(_:_) =
   putStrLn header >> mapM_ (processFile options) files

processFile :: Options -> FilePath -> IO ()
processFile options@(Options {..}) filePath = do
   sequences <- readFasta filePath
   putStrLn $ prettyOutput filePath $ sequenceStats options sequences 

sequenceStats :: Options -> [Sequence] -> Maybe Stats
sequenceStats (Options {..}) sequences =
   case filteredLengths of
      [] -> Nothing
      first:rest -> 
        Just $ foldl' updateStats (initStats first) rest 
   where
   filteredLengths =
      filter (\x -> sequenceLengthInteger x >= minLengthThreshold) sequences

prettyOutput :: FilePath -> Maybe Stats -> String
prettyOutput filePath Nothing =
   filePath ++ "\t0\t0\t-\t-\t-"
prettyOutput filePath (Just stats@(Stats {..})) =
   concat $ intersperse "\t" (filePath : numbers)
   where
   average
      | numSequences > 0 =
           show $ floor (fromIntegral numBases / fromIntegral numSequences)
      | otherwise = "-"
   numbers = [ show numSequences, show numBases, show minSequenceLength
             , average, show maxSequenceLength ]

data Stats =
   Stats 
   { numSequences :: !Integer
   , numBases :: !Integer
   , minSequenceLength :: !Integer
   , maxSequenceLength :: !Integer
   }
   deriving (Eq, Ord, Show)

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
