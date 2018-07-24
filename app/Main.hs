{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import Lib
import PgnGame 
import Data.Attoparsec.ByteString.Char8

-- | File where the games are stored.
logFile :: FilePath
logFile = "PgnGames.pgn"

main :: IO ()
main = do
  file1 <- B.readFile logFile
  let r =  parseOnly pgnDatabaseParser file1
  case r of
    Left err -> putStrLn $ "A parsing error was found: " ++ err
    Right log -> let
                   fens = map (tagValue . head . filter (\tp -> tagName tp == Fen) . tagSection) log
                   -- ac = someFunc fens
                 in someFunc fens 
