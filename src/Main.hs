{-# LANGUAGE OverloadedStrings #-}

import System.IO (openFile, hClose, IOMode(..))
import Options.Applicative
import Data.ByteString.Lazy (ByteString)
import Data.Binary (encode)
import Data.Digest.CRC32 (crc32)
import Control.Error (runScript, hoistEither, scriptIO)
import qualified Data.ByteString.Lazy as B

import Options

pngHeaderSize :: Int
pngHeaderSize = 8 -- bytes

ihdrChunkSize :: Int
ihdrChunkSize = 25 -- 4 bytes length, 4 bytes identity, 13 bytes data, 4 bytes crc

verifyPNGHeader :: ByteString -> Either String ()
verifyPNGHeader h = if h == expectedHeader
                      then Right () 
                      else Left "Incorrect PNG file provided (header mismatch)"
  where expectedHeader = B.pack . map fromIntegral $ ([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]::[Integer])

verifyIHDRChunk :: ByteString -> Either String ()
verifyIHDRChunk h = if B.drop 4 (B.take 8 h) == expectedHeaderType
                      then Right ()
                      else Left "Incorrect PNG file provided (IHDR chunk not found)"
  where expectedHeaderType = B.pack [73, 72, 68, 82]

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
genereateTEXTChunk options = lengthChunk `B.append` typeChunk `B.append` dataChunk `B.append` crcChunk
  where lengthChunk = encode (sizeIncrease options - 12)
        typeChunk = B.pack [116, 69, 88, 116] -- tEXt
        dataChunk = B.pack [70, 0] `B.append` B.replicate (fromIntegral (sizeIncrease options) - 2 - 12) 87
        crcChunk = encode $ crc32 (typeChunk `B.append` dataChunk)

-- append tEXt chunk right after IHDR chunk
appendChunkToFile :: Options -> ByteString -> IO ()
appendChunkToFile options chunk = do
    inputHandle <- openFile (inputFile options) ReadWriteMode
    outputHandle <- openFile (outputFile options) WriteMode
    B.hPut outputHandle =<< B.hGet inputHandle (pngHeaderSize + ihdrChunkSize)
    B.hPut outputHandle chunk
    B.hPut outputHandle =<< B.hGetContents inputHandle
    hClose inputHandle
    hClose outputHandle

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
