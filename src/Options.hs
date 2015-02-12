module Options where

import Data.Word (Word32)
import Options.Applicative

data Options = Options { inputFile    :: FilePath
                       , outputFile   :: FilePath
                       , sizeIncrease :: Word32
                       }

optionsParser :: Parser Options
optionsParser = Options
            <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input PNG file")
            <*> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output PNG file")
            <*> option auto (short 's' <> metavar "SIZE" <> help "Size increase in bytes")
