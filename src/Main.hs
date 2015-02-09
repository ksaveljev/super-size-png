{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Data.ByteString (ByteString)
import Control.Error (runScript, hoistEither)
import qualified Data.ByteString.Char8 as B

import Options

verifyOptions :: Options -> Either String ()
verifyOptions options =
    if sizeIncrease options < 10
      then Left "OH OH"
      else Right ()

increasePNGSize :: Options -> IO ()
increasePNGSize options = runScript $ do
  hoistEither $ verifyOptions options -- there is a limit on minimum size increase
  undefined
  {-
  verifyInputFileHeader -- PNG header verification
  genereateTEXTChunk -- bytestring with desired length
  appendChunkToFile
  -}

main :: IO ()
main = execParser opts >>= increasePNGSize
  where opts = info (helper <*> optionsParser)
                    (fullDesc <> progDesc "Increase size of a PNG input file by SIZE bytes" 
                              <> header "super-size-png - a tool for increasing size of a PNG file")
