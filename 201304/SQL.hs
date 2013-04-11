{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite3
import Data.Text

dbname = "chinook.sqlite"
query  = "select * from album;"

main :: IO ()
main = do
    db <- open dbname
    statement <- prepare db query
    processRows statement

processRows :: Statement -> IO ()
processRows statement = do
    stepResult <- step statement
    case stepResult of
      Row -> do
          cols <- columns statement
          print cols
          processRows statement
      Done ->
          putStrLn "All done"
