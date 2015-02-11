{-# LANGUAGE OverloadedStrings #-}

import Data.Char (chr)
import System.IO (openFile, hClose, IOMode(ReadMode))
import Options.Applicative
import Data.ByteString (ByteString)
import Control.Error (runScript, hoistEither, scriptIO)
import qualified Data.ByteString.Char8 as B

import Options

pngHeaderSize :: Int
pngHeaderSize = 8 -- bytes

ihdrChunkSize :: Int
ihdrChunkSize = 25 -- 4 bytes length, 4 bytes identity, 13 bytes data, 4 bytes crc

verifyPNGHeader :: ByteString -> Either String ()
verifyPNGHeader h = if h == expectedHeader
                      then Right () 
                      else Left "Incorrect PNG file provided (header mismatch)"
  where expectedHeader = B.pack . map chr $ [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

verifyIHDRChunk :: ByteString -> Either String ()
verifyIHDRChunk = undefined -- TODO

-- keyword = 1 byte
-- null separator = 1 byte
-- text = n bytes [0..]
--
-- each chunk:
-- 4-byte unsigned integer giving the number of bytes in the chunk's data
-- field (zero is a valid length)
-- 4-byte chunk type (tEXt)
-- X-byte chunk data
-- 4-byte CRC
--
-- One notable restriction is that IHDR must appear first and IEND must appear last;
-- thus the IEND chunk serves as an end-of-file marker
verifyOptions :: Options -> Either String ()
verifyOptions options =
    if sizeIncrease options < 14 -- 4 bytes length, 4 bytes identity, min 2 bytes data, 4 bytes crc
      then Left "Unfortunately increasing size by less than 14 bytes is not supported"
      else Right ()

genereateTEXTChunk :: Options -> ByteString
genereateTEXTChunk = undefined -- TODO

-- append tEXt chunk right after IHDR chunk
appendChunkToFile :: Options -> ByteString -> IO ()
appendChunkToFile = undefined -- TODO

increasePNGSize :: Options -> IO ()
increasePNGSize options = runScript $ do
  (fileHeader, ihdrChunk) <- scriptIO $ do
    handle <- openFile (inputFile options) ReadMode
    h <- B.hGet handle pngHeaderSize
    ihdr <- B.hGet handle ihdrChunkSize
    hClose handle
    return (h, ihdr)
  hoistEither $ do
    verifyOptions options
    verifyPNGHeader fileHeader
    verifyIHDRChunk ihdrChunk
  let textChunk = genereateTEXTChunk options
  scriptIO $ appendChunkToFile options textChunk
  return ()

main :: IO ()
main = execParser opts >>= increasePNGSize
  where opts = info (helper <*> optionsParser)
                    (fullDesc <> progDesc "Increase size of a PNG input file by SIZE bytes" 
                              <> header "super-size-png - a tool for increasing size of a PNG file")
