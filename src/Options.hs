module Options where

import Options.Applicative

data Options = Options { inputFile    :: FilePath
                       , outputFile   :: Maybe FilePath
                       , sizeIncrease :: Int
                       }

optionsParser :: Parser Options
optionsParser = Options
            <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input PNG file")
            <*> strOptional (long "output" <> short 'o' <> metavar "FILE" <> help "Output PNG file")
            <*> option auto (short 's' <> metavar "SIZE" <> help "Size increase in bytes")

strOptional :: Mod OptionFields String -> Parser (Maybe String)
strOptional flags = optional (strOption flags)
