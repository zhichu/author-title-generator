module Main where

import JACoW.Types
import qualified Data.Yaml as Y
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BS

main = do
  ymlData <- BS.readFile "mypaper.yaml"
  let authortitle = Y.decodeEither' ymlData :: Either Y.ParseException AuthorTitle
  TIO.writeFile "mypaper-hs.tex" $ either (error . show) simpletitle authortitle
