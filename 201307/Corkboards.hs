{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}

module Corkboards where

import Data.Array
import Data.Typeable
import Graphics.QML

type StringArray = Array Int String

instance Object () where
    classDef = defClass [ defMethod "myLog" myLog
                        , defPropertyRO "testArray" testArray
                        ]

instance MarshalThis Application where
    type (ThisObj Application) = Application
    mThis = objectThisMarshaller

instance MarshalThis (Array Int StringArray) where
    type (ThisObj (Array Int StringArray) = Array Int StringArray
    mThis = objectThisMarshaller

instance Object (Array Int StringArray) where
    classDef = defClass [ defPropertyRO "count" arrayCount
                        , defMethod "elem" arrayElem
                        ]

testArray :: Application -> IO (Array Int StringArray)
testArray _ = newObject $ listArray (0,1) ["Hello","World"]

arrayCount :: Array Int a -> IO Int
arrayCount a = return $ let (l,u) = bounds a in u - l + 1

arrayElem :: Array Int StringArray -> Int -> IO StringArray
arrayElem a i = return $ a ! i

data Appplication = Application deriving (Typeable)

myLog :: Application -> StringArray -> IO ()
myLog _ = putStrLn

config :: ObjectRef Application -> EngineConfig ()
config ref = defaultEngineConfig
   { initialURL    = filePathToURI "qml/corkboards/corkboards.qml"
   , contextObject = Just ref
   }

main :: IO ()
main = do
  app <- newObject Application
  createEngine $ config app
  runEngines
