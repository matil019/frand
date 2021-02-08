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
  <$> optional (strOption (long "seed-in" <> metavar "FILE" <> help "RNG seed file"))
  <*> optional (strOption (long "seed-out" <> metavar "FILE" <> help "File to output seed after generating a number"))
