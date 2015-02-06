{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Options

outputResult :: Either ByteString ByteString -> IO ()
outputResult (Left errorMessage) = B.putStr "Error: " >> B.putStrLn errorMessage
outputResult (Right message) = B.putStrLn message

increaseSize :: Options -> IO (Either ByteString ByteString)
increaseSize = undefined

main :: IO ()
main = execParser opts >>= increaseSize >>= outputResult
  where opts = info (helper <*> optionsParser)
                    (fullDesc <> progDesc "Increase size of a PNG input file by SIZE bytes" 
                              <> header "super-size-png - a tool for increasing size of a PNG file")
