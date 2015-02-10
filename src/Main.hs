{-# LANGUAGE OverloadedStrings #-}

import Data.Char (chr)
import System.IO (openFile, IOMode(ReadMode))
import Options.Applicative
import Data.ByteString (ByteString)
import Control.Error (runScript, hoistEither, scriptIO)
import qualified Data.ByteString.Char8 as B

import Options

pngHeaderSize :: Int
pngHeaderSize = 8 -- bytes

verifyPNGHeader :: ByteString -> Either String ()
verifyPNGHeader h = if h == expectedHeader
                      then Right () 
                      else Left "Incorrect PNG file provided (header mismatch)"
  where expectedHeader = B.pack . map chr $ [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

verifyOptions :: Options -> Either String ()
verifyOptions options =
    if sizeIncrease options < 10 -- TODO: need to calculate what is the minimum size increase we can allow
      then Left "OH OH"
      else Right ()

genereateTEXTChunk :: Options -> ByteString
genereateTEXTChunk = undefined -- TODO

appendChunkToFile :: Options -> ByteString -> IO ()
appendChunkToFile = undefined -- TODO

increasePNGSize :: Options -> IO ()
increasePNGSize options = runScript $ do
  hoistEither $ verifyOptions options -- there is a limit on minimum size increase
  fileHeader <- scriptIO $ do
    handle <- openFile (inputFile options) ReadMode
    B.hGet handle pngHeaderSize
  hoistEither $ verifyPNGHeader fileHeader
  let textChunk = genereateTEXTChunk options
  scriptIO $ appendChunkToFile options textChunk
  return ()

main :: IO ()
main = execParser opts >>= increasePNGSize
  where opts = info (helper <*> optionsParser)
                    (fullDesc <> progDesc "Increase size of a PNG input file by SIZE bytes" 
                              <> header "super-size-png - a tool for increasing size of a PNG file")
