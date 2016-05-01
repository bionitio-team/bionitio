module Main where

import Lib
import Bio.Sequence.Fasta as Fasta
import Options.Applicative 
   ( Parser, option, auto, long, metavar, help, value
   , some, argument, str, info, execParser, switch, fullDesc, (<*>), (<$>) , (<>)
   , helper, progDesc )
import Data.List (foldl', intersperse)


defaultMinLen :: Integer
defaultMinLen = 0

data Options = Options
    { minlen :: Integer
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
            <> help ("Minimum length sequence to include in stats (default=" ++ show defaultMinLen ++ ")")
            <> value defaultMinLen)
    <*> switch (long "version" <> help "Print version and exit")
    <*> switch (long "verbose" <> help "Print more stuff about what's happening")
    <*> some (argument str (metavar "FASTA_FILE [FASTA_FILE ...]"))

main :: IO ()
main = do
    options <- execParser opts
    processFastaFiles $ fastaFiles options
    where
    opts = info (helper <*> defineOptions)
                (fullDesc <> progDesc "Print fasta stats")

header :: String
header = "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX"

processFastaFiles :: [FilePath] -> IO ()
processFastaFiles files = do
    putStrLn header
    mapM_ processFile files

processFile :: FilePath -> IO ()
processFile filePath = do
    sequences <- readFasta filePath
    let stats = foldl' updateStats initStats sequences
        average = round ((fromIntegral $ numBases stats) / (fromIntegral $ numSequences stats))
        result = concat $ intersperse "\t"
                [ filePath, show (numSequences stats), show (numBases stats)
                , pretty (minSequenceLength stats), show average, pretty (maxSequenceLength stats) ]
    putStrLn result 

pretty :: Show a => Maybe a -> String
pretty Nothing = "N/A"
pretty (Just x) = show x

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

updateStats :: Stats -> Sequence -> Stats
updateStats oldStats sequence =
    Stats { numSequences = newNumSequences
          , numBases = newNumBases
          , minSequenceLength = newMinSequenceLength
          , maxSequenceLength = newMaxSequenceLength }
    where
    thisLength = fromIntegral $ seqlength sequence
    newNumSequences = (numSequences oldStats) + 1
    newNumBases = (numBases oldStats) + thisLength 
    newMinSequenceLength =
        case minSequenceLength oldStats of
            Nothing -> Just thisLength
            orig@(Just oldMin)
                | thisLength < oldMin -> Just thisLength
                | otherwise -> orig
    newMaxSequenceLength =
        case maxSequenceLength oldStats of
            Nothing -> Just thisLength
            orig@(Just oldMax)
                | thisLength > oldMax -> Just thisLength
                | otherwise -> orig


{-
biotool -h
Synposis:
  Print fasta stats
Usage:
  biotool [options] contigs.fasta [another.fa ...]
Options:
  --help       Show this help
  --version    Print version and exit
  --verbose    Print more stuff about what's happening
  --minlen N   Minimum length sequence to include in stats (default=0)

One file parameter:

% biotool file.fa
FILENAME  TOTAL  NUMSEQ   MIN  AVG  MAX
file.fa   5264   3801855  31   722  53540
Multiple files:

% biotool file1.fa file2.fa file3.fa
FILENAME   TOTAL  NUMSEQ   MIN  AVG  MAX
file1.fa   5264   3801855  31   722  53540
file2.fa   5264   3801855  31   722  53540
file3.fa   5264   3801855  31   722  53540
Standard input:

% biotool < file.fa
FILENAME  TOTAL  NUMSEQ   MIN  AVG  MAX
stdin     5264   3801855  31   722  53540
Restricted length:

% biotool --minlen 1000 file.fa
FILENAME  TOTAL  NUMSEQ   MIN    AVG  MAX
file.fa   4711   2801855  1021   929  53540
-}
