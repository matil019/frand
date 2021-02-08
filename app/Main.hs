{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative ((<|>), optional)
import Options.Applicative (Parser, customExecParser)
import Options.Applicative qualified as O
import System.Random.MWC (createSystemRandom, uniformRM)
import System.Random.MWC.Distributions (normal)

data UniformArgs = UniformArgs
  { minincl :: Double
  , maxincl :: Double
  -- TODO seedIn, seedOut
  }
  deriving (Eq, Show)

uniformArgsParser :: Parser UniformArgs
uniformArgsParser = namedParser <|> positionalParser
  where
  namedParser = (\minincl maxincl -> UniformArgs{..})
    <$> O.option O.auto (O.long "min" <> O.metavar "MIN" <> O.help "Minimum value, inclusive")
    <*> O.option O.auto (O.long "max" <> O.metavar "MAX" <> O.help "Maximum value, inclusive")

  positionalParser = (\minincl maxincl -> UniformArgs{..})
    <$> O.argument O.auto (O.metavar "MIN")
    <*> O.argument O.auto (O.metavar "MAX")

data NormalArgs = NormalArgs
  { mean    :: Double
  , stddev  :: Double
  , seedIn  :: Maybe FilePath
  , seedOut :: Maybe FilePath
  }
  deriving (Eq, Show)

normalArgsParser :: Parser NormalArgs
normalArgsParser =
  (\(seedIn, seedOut) (mean, stddev) -> NormalArgs{..})
  <$> optionsParser
  <*> (namedParser <|> positionalParser)
  where
  optionsParser = (,)
    <$> optional (O.strOption (O.long "seed-in" <> O.metavar "FILE" <> O.help "RNG seed file"))
    <*> optional (O.strOption (O.long "seed-out" <> O.metavar "FILE" <> O.help "File to output seed after generating a number"))

  namedParser = (,)
    <$> O.option O.auto (O.long "mean"   <> O.metavar "MEAN"   <> O.help "Mean")
    <*> O.option O.auto (O.long "stddev" <> O.metavar "STDDEV" <> O.help "Standard deviation")

  positionalParser = (,)
    <$> O.argument O.auto (O.metavar "MEAN")
    <*> O.argument O.auto (O.metavar "STDDEV")

data Distribution = Uniform UniformArgs | Normal NormalArgs
  deriving (Eq, Show)

versionFlag :: Parser (a -> a)
versionFlag = O.infoOption VERSION_frand (O.long "version" <> O.help "Show the version")

main :: IO ()
main = do
  args <- customExecParser (O.prefs O.showHelpOnError) $
    let allOpts = O.hsubparser
          ( O.command "uniform" (O.info (Uniform <$> uniformArgsParser) (O.progDesc "Print a random number in uniform distribution"))
         <> O.command "normal"  (O.info (Normal  <$> normalArgsParser ) (O.progDesc "Print a random number in normal distribution"))
          )
    in O.info (versionFlag <*> O.helper <*> allOpts)
       ( O.fullDesc
      <> O.progDesc "Print a random decimal number"
       )
  let distr = case args of
        Uniform UniformArgs{minincl, maxincl} -> uniformRM (minincl, maxincl)
        Normal  NormalArgs{mean, stddev} -> normal mean stddev
  print =<< distr =<< createSystemRandom
