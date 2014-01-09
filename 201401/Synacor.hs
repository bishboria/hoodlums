{-# LANGUAGE RecordWildCards #-}

module Synacor where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Char
import qualified Data.IntMap.Strict as M
import           Data.Word

data Machine = Machine
  {
    mem   :: !(M.IntMap Word16)
  , stack :: [Word16]
  , ip    :: Int
  }

type Syn = State Machine

run :: Machine -> String -> String
run m i = evalState (run' i) m

run' :: String -> Syn String
run' input = getMem 0 >>= \inst -> case inst of
  0  -> return []
  19 -> do
    c <- chr . fromIntegral <$> getValue 1
    incip 2
    (c:) <$> run' input
  20 -> do
    let (c:input') = input
    addr <- getAddr 1
    store addr (fromIntegral $ ord c)
    incip 2
    run' input'
  21 -> incip 1 >> run' input

incip :: Int -> Syn ()
incip i = modify $ \s -> s{ip = ip s + i}

store :: Int -> Word16 -> Syn ()
store addr value = modify $ \s -> s{mem = M.insert addr value (mem s)}

getMem :: Int -> Syn Word16
getMem offset = (M.!) <$> gets mem <*> gets ((+ offset) . ip)

getAddr :: Int -> Syn Int
getAddr offset = fromIntegral <$> getMem offset

getValue :: Int -> Syn Word16
getValue n = do
    i <- getMem n
    if i > 32767 then retrieve (fromIntegral i) else return i

retrieve :: Int -> Syn Word16
retrieve addr = (M.! addr) <$> gets mem

load :: [Word16] -> Machine
load prog = Machine (M.fromList $ zip [0..] prog) [] 0
