{-# LANGUAGE OverloadedStrings #-}

import System.IO (openFile, hClose, IOMode(..))
import Options.Applicative
import Data.ByteString.Lazy (ByteString)
import Data.Binary (encode)
import Data.Digest.CRC32 (crc32)
import Control.Error (runScript, hoistEither, scriptIO)
import qualified Data.ByteString.Lazy as B

import Options

-- PNG header has fixed size and we are going to use that fact in our
-- calculations
pngHeaderSize :: Int
pngHeaderSize = 8 -- bytes

-- IHDR chunk is the first chunk following PNG header and also has a fixed
-- size which will also help us in our calculations
ihdrChunkSize :: Int
ihdrChunkSize = 25 -- 4 bytes length, 4 bytes identity, 13 bytes data, 4 bytes crc

-- PNG header should be the same for all image files and we want to make
-- sure that the provided input file at least has a correct header...
verifyPNGHeader :: ByteString -> Either String ()
verifyPNGHeader h = if h == expectedHeader
                      then Right () 
                      else Left "Incorrect PNG file provided (header mismatch)"
  where expectedHeader = B.pack . map fromIntegral $ ([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]::[Integer])

-- ... and that the first chunk following the PNG header is a chunk of IHDR
-- type (here we only check the signature of the chunk and nothing else)
verifyIHDRChunk :: ByteString -> Either String ()
verifyIHDRChunk h = if B.drop 4 (B.take 8 h) == expectedHeaderType
                      then Right ()
                      else Left "Incorrect PNG file provided (IHDR chunk not found)"
  where expectedHeaderType = B.pack [73, 72, 68, 82]

-- The only thing we disallow is for our increase size to be smaller than
-- 14 bytes. This is due to the fact that adding a chunk with length
-- 0 results in 12 bytes (4 bytes for length, 4 bytes for type and 4 bytes
-- crc). But if we decide to add and empty value to tEXt key/value chunk
-- then we end up with a minimum of 14 bytes increase
verifyOptions :: Options -> Either String ()
verifyOptions options =
    if sizeIncrease options < 14 -- 4 bytes length, 4 bytes identity, min 2 bytes data, 4 bytes crc
      then Left "Unfortunately increasing size by less than 14 bytes is not supported"
      else Right ()

-- We are given the desired size increase and we need to calcuate what kind
-- of data we need to generate based on those numbers:
-- 4 bytes is for the length of the chunk
-- 4 bytes is for the type of the chunk (tEXt)
-- 4 bytes is for the crc32 of the chunk
-- That is why you see -12 in these calculations
genereateTEXTChunk :: Options -> ByteString
genereateTEXTChunk options = lengthChunk `B.append` typeChunk `B.append` dataChunk `B.append` crcChunk
  where lengthChunk = encode (sizeIncrease options - 12)
        typeChunk = B.pack [116, 69, 88, 116] -- tEXt
        dataChunk = B.pack [70, 0] `B.append` B.replicate (fromIntegral (sizeIncrease options) - 2 - 12) 87
        crcChunk = encode $ crc32 (typeChunk `B.append` dataChunk)

-- Open an input file and read its header and IHDR chunk and write them to
-- the output file. Then add our tEXt chunk to the output file and then
-- read the rest from the input file and write it to the output file
-- resulting in a PNG with a tEXt chunk of desired length right after the
-- IHDR chunk
appendChunkToFile :: Options -> ByteString -> IO ()
appendChunkToFile options chunk = do
    inputHandle <- openFile (inputFile options) ReadWriteMode
    outputHandle <- openFile (outputFile options) WriteMode
    B.hPut outputHandle =<< B.hGet inputHandle (pngHeaderSize + ihdrChunkSize)
    B.hPut outputHandle chunk
    B.hPut outputHandle =<< B.hGetContents inputHandle
    hClose inputHandle
    hClose outputHandle

-- Do some simple verifications before we proceed to actually adding data
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
