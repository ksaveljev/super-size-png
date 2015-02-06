import Options.Applicative

import Options

main :: IO ()
main = execParser opts >>= undefined
  where opts = info (helper <*> optionsParser)
                    (fullDesc <> progDesc "Increase size of a PNG input file by SIZE bytes" 
                              <> header "super-size-png - a tool for increasing size of a PNG file")
