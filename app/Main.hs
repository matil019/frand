{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import CmdArgs (CommonArgs, commonArgsParser, numRand, seedIn, seedOut)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Monad (replicateM_)
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Vector.Storable qualified as VS
import Options.Applicative (Parser, customExecParser)
import Options.Applicative qualified as O
import System.Random.MWC (createSystemRandom, uniformRM)
import System.Random.MWC qualified as MWC
import System.Random.MWC.Distributions (normal)

data UniformArgs = UniformArgs
  { minincl :: Double
  , maxincl :: Double
  }
  deriving (Eq, Show)

uniformArgsParser :: Parser (CommonArgs, UniformArgs)
uniformArgsParser = (,) <$> commonArgsParser <*> (namedParser <|> positionalParser)
  where
  namedParser = (\minincl maxincl -> UniformArgs{..})
    <$> O.option O.auto (O.long "min" <> O.metavar "MIN" <> O.help "Minimum value, inclusive")
    <*> O.option O.auto (O.long "max" <> O.metavar "MAX" <> O.help "Maximum value, inclusive")

  positionalParser = (\minincl maxincl -> UniformArgs{..})
    <$> O.argument O.auto (O.metavar "MIN")
    <*> O.argument O.auto (O.metavar "MAX")

data NormalArgs = NormalArgs
  { mean   :: Double
  , stddev :: Double
  }
  deriving (Eq, Show)

normalArgsParser :: Parser (CommonArgs, NormalArgs)
normalArgsParser = (,) <$> commonArgsParser <*> (namedParser <|> positionalParser)
  where
  namedParser = (\mean stddev -> NormalArgs{..})
    <$> O.option O.auto (O.long "mean"   <> O.metavar "MEAN"   <> O.help "Mean")
    <*> O.option O.auto (O.long "stddev" <> O.metavar "STDDEV" <> O.help "Standard deviation")

  positionalParser = (\mean stddev -> NormalArgs{..})
    <$> O.argument O.auto (O.metavar "MEAN")
    <*> O.argument O.auto (O.metavar "STDDEV")

data Distribution = Uniform UniformArgs | Normal NormalArgs
  deriving (Eq, Show)

versionFlag :: Parser (a -> a)
versionFlag = O.infoOption VERSION_frand (O.long "version" <> O.help "Show the version")

roundUp :: Int -> Int -> Int
roundUp r n = (n + (r - 1)) `div` r * r

main :: IO ()
main = do
  (commonArgs, distrArgs) <- customExecParser (O.prefs O.showHelpOnError) $
    let allOpts = O.hsubparser
          ( O.command "uniform" (O.info (second Uniform <$> uniformArgsParser) (O.progDesc "Print a random number in uniform distribution"))
         <> O.command "normal"  (O.info (second Normal  <$> normalArgsParser ) (O.progDesc "Print a random number in normal distribution"))
          )
    in O.info (versionFlag <*> O.helper <*> allOpts)
       ( O.fullDesc
      <> O.progDesc "Print a random decimal number"
       )

  let distr = case distrArgs of
        Uniform UniformArgs{minincl, maxincl} -> uniformRM (minincl, maxincl)
        Normal  NormalArgs{mean, stddev} -> normal mean stddev

  g <- maybe createSystemRandom readSeedFile $ seedIn commonArgs
  replicateM_ (numRand commonArgs) $
    print =<< distr g
  maybe (pure ()) (flip writeSeedFile g) $ seedOut commonArgs

  where
  -- I'm too lazy to convert Word8s to Word32s by hand; I just use casts here as well as writeSeedFile.
  -- On reading, NUL bytes are padded as necessary.
  readSeedFile fp = do
    -- TODO read no more than 258 * 4 = 1032 bytes, because mwc-random uses at most 258 Word32s?
    bs <- B.readFile fp
    let word8s = VS.unfoldrN (roundUp 4 $ B.length bs) (\i -> Just (fromMaybe 0 $ bs B.!? i, succ i)) 0
        word32s = VS.unsafeCast word8s
    MWC.initialize word32s

  writeSeedFile fp g = do
    word32sU <- MWC.fromSeed <$> MWC.save g
    let word32sS = VS.convert word32sU
        word8s = VS.unsafeCast word32sS
        bs = B.unfoldr VS.uncons word8s
    B.writeFile fp bs
