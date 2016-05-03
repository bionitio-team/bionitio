{-
 - TODO: 
 -     handle fasta file from stdin
 -     ignore sequences with len < minlen
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
    options <- execParser optionParser 
    processFastaFiles $ fastaFiles options
    where
    optionParser =
        info (helper <*> defineOptions)
             (fullDesc <> progDesc "Print fasta stats")

header :: String
header = "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX"

processFastaFiles :: [FilePath] -> IO ()
processFastaFiles files = putStrLn header >> mapM_ processFile files

processFile :: FilePath -> IO ()
processFile filePath = do
    sequences <- readFasta filePath
    case sequences of
        [] -> return ()
        (firstSeq:restSeqs) -> do
            let stats = foldl' updateStats (initStats firstSeq) restSeqs
            putStrLn $ prettyOutput filePath stats 

prettyOutput :: FilePath -> Stats -> String
prettyOutput filePath stats@(Stats {..}) =
    concat $ intersperse "\t" (filePath : numbers)
    where
    average = round (fromIntegral numBases / 
                     fromIntegral numSequences)
    numbers = map show [ numSequences
                       , numBases
                       , minSequenceLength
                       , average
                       , maxSequenceLength]

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
    thisLength = fromIntegral $ seqlength sequence

updateStats :: Stats -> Sequence -> Stats
updateStats (Stats {..}) sequence =
    Stats newNumSequences newNumBases
          newMinSequenceLength newMaxSequenceLength 
    where
    newNumSequences = numSequences + 1
    thisLength = fromIntegral $ seqlength sequence
    newNumBases = numBases + thisLength 
    newMinSequenceLength
        | thisLength < minSequenceLength = thisLength
        | otherwise = minSequenceLength 
    newMaxSequenceLength
        | thisLength > maxSequenceLength = thisLength
        | otherwise = maxSequenceLength 
