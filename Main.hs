{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM

import Yesod
import System.Directory
import Control.Monad

import Dispatch ()
import Foundation



main :: IO ()
main = do
    tfilenames <- atomically $ newTVar []
    fe<-doesFileExist "test5.db3"
    when (not fe) initDB
    warpEnv $ App tfilenames
