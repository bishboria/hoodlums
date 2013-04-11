{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Database.SQLite3
import Data.Text
import Data.Int (Int64)
import Data.Function (fix)

data Album = Album {
      albumIdx       :: Int64
    , albumTitle     :: Text
    , albumArtistIdx :: Int64
    } deriving (Show)

class HasHandler h where
  getHandler :: h -> Statement -> ColumnIndex -> IO ()

instance HasHandler (IO ()) where
  getHandler h _ _ = h

instance (HasHandler a) => HasHandler (Int64 -> a) where
  getHandler f s i = do
    v <- columnInt64 s i
    getHandler (f v) s (i + 1)

instance (HasHandler a) => HasHandler (Text -> a) where
  getHandler f s i = do
    v <- columnText s i
    getHandler (f v) s (i + 1)

dbname = "chinook.sqlite"
query  = "select * from album;"

main :: IO ()
main = do
    db <- open dbname
    execQuery db query processRow

processRow :: Int64 -> Text -> Int64 -> IO ()
processRow idx title artistIdx = print $ Album idx title artistIdx

execQuery :: (HasHandler a) => Database -> Text -> a -> IO ()
execQuery db q rowProcessor = do
    statement <- prepare db q
    fix $ \loop -> do
        stepResult <- step statement
        case stepResult of
          Row -> do
              getHandler rowProcessor statement 0
              loop
          Done -> return ()
