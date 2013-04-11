{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite3
import Data.Text
import Data.Int (Int64)

data Album = Album {
      albumIdx       :: Int64
    , albumTitle     :: Text
    , albumArtistIdx :: Int64
    } deriving (Show)

dbname = "chinook.sqlite"
query  = "select * from album;"

main :: IO ()
main = do
    db <- open dbname
    statement <- prepare db query
    untilM_ testDone $ processRow statement

untilM_ :: (a -> Bool) -> IO a -> IO ()
untilM_ p a = do
    r <- a
    if p r
       then return ()
       else untilM_ p a

testDone :: StepResult -> Bool
testDone Done = True
testDone Row  = False

processRow :: Statement -> IO StepResult
processRow statement = do
    stepResult <- step statement
    case stepResult of
        Row -> do
            idx <- columnInt64 statement 0
            title <- columnText statement 1
            artistIdx <- columnInt64 statement 2
            print $ Album idx title artistIdx
        Done -> putStrLn "All done"
    return stepResult
