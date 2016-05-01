module Main where

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
        -- XXX check the output of average. Also make sure denominator is not zero
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

-- XXX this needs cleaning up
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
