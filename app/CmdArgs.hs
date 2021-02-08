{-# LANGUAGE RecordWildCards #-}
module CmdArgs where

import Control.Monad (when)
import Options.Applicative

data CommonArgs = CommonArgs
  { seedIn  :: Maybe FilePath
  , seedOut :: Maybe FilePath
  , numRand :: Int
  }
  deriving (Eq, Show)

nonNegIntReader :: ReadM Int
nonNegIntReader = do
  i <- auto
  when (i < 0) $ fail "must be a non-negative integer"
  pure i

commonArgsParser :: Parser CommonArgs
commonArgsParser = (\seedIn seedOut numRand -> CommonArgs{..})
  <$> optional (strOption (long "seed-in" <> metavar "FILE" <> help "RNG seed file (for reproducible random number)"))
  <*> optional (strOption (long "seed-out" <> metavar "FILE" <> help "File to output seed after generating a number. Can be the same file as --seed-in, in which case it is overwritten with a new seed"))
  <*> option nonNegIntReader (long "num" <> metavar "INT" <> value 1 <> showDefault <> help "Number of random numbers to generate")
