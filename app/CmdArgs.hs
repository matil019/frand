{-# LANGUAGE RecordWildCards #-}
module CmdArgs where

import Options.Applicative

data SeedArgs = SeedArgs
  { seedIn  :: Maybe FilePath
  , seedOut :: Maybe FilePath
  }
  deriving (Eq, Show)

seedArgsParser :: Parser SeedArgs
seedArgsParser = (\seedIn seedOut -> SeedArgs{..})
  <$> optional (strOption (long "seed-in" <> metavar "FILE" <> help "RNG seed file (for reproducible random number)"))
  <*> optional (strOption (long "seed-out" <> metavar "FILE" <> help "File to output seed after generating a number. Can be the same file as --seed-in, in which case it is overwritten with a new seed"))
