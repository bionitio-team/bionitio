{-
 - TODO: 
 -    handle fasta file from stdin
 -    ignore sequences with len < minlenThreshold
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import Bio.Sequence.Fasta
   ( readFasta, seqlength, Sequence )
import Options.Applicative 
   ( Parser, option, auto, long, metavar, help, value
   , some, argument, str, info, execParser, switch
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
   <*> some (argument str (metavar "FASTA_FILE [FASTA_FILE ...]"))

main :: IO ()
main = do
   options <- execParser optionParser 
   processFastaFiles options $ fastaFiles options
   where
   optionParser =
      info (helper <*> defineOptions)
           (fullDesc <> progDesc "Print fasta stats")

header :: String
header = "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX"

processFastaFiles :: Options -> [FilePath] -> IO ()
processFastaFiles options files =
   putStrLn header >> mapM_ (processFile options) files

processFile :: Options -> FilePath -> IO ()
processFile options@(Options {..}) filePath = do
   sequences <- readFasta filePath
   let stats = foldl' (updateStats options) initStats sequences 
   putStrLn $ prettyOutput filePath stats 

prettyOutput :: FilePath -> Stats -> String
prettyOutput filePath stats@(Stats {..}) =
   concat $ intersperse "\t" (filePath : numbers)
   where
   average
      | numSequences > 0 =
           Just $ round (fromIntegral numBases / fromIntegral numSequences)
      | otherwise = Nothing
   numbers = [ show numSequences
             , show numBases
             , prettyMaybe minSequenceLength
             , prettyMaybe average
             , prettyMaybe maxSequenceLength ]
   prettyMaybe :: Show a => Maybe a -> String
   prettyMaybe Nothing = "-"
   prettyMaybe (Just x) = show x

data Stats =
   Stats 
   { numSequences :: !Integer
   , numBases :: !Integer
   , minSequenceLength :: !(Maybe Integer)
   , maxSequenceLength :: !(Maybe Integer)
   }
   deriving (Eq, Ord, Show)

initStats :: Stats
initStats = Stats
   { numSequences = 0
   , numBases = 0 
   , minSequenceLength = Nothing 
   , maxSequenceLength = Nothing 
   }

sequenceLengthInteger :: Sequence -> Integer
sequenceLengthInteger = fromIntegral . seqlength

updateStats :: Options -> Stats -> Sequence -> Stats
updateStats (Options {..}) oldStats@(Stats {..}) sequence
   | thisLength < minLengthThreshold = oldStats
   | otherwise =
        Stats newNumSequences newNumBases
              newMinSequenceLength newMaxSequenceLength 
   where
   newNumSequences = numSequences + 1
   thisLength = sequenceLengthInteger sequence
   newNumBases = numBases + thisLength 
   newMinSequenceLength
      | Just oldMinLength <- minSequenceLength =
           if thisLength < oldMinLength
              then Just thisLength else minSequenceLength
      | otherwise = Just thisLength 
   newMaxSequenceLength
      | Just oldMaxLength <- maxSequenceLength =
           if thisLength > oldMaxLength
              then Just thisLength else maxSequenceLength
      | otherwise = Just thisLength 
