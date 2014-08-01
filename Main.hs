{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM

import Yesod
import System.Directory
import Control.Monad
import           Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import           Yesod.Auth
import Dispatch ()
import Foundation
import Data.Text (unpack)


main :: IO ()
main = do
    fe<-doesFileExist (unpack bunnyLuvDB)
    tport <- getPort
    let prt = read (unpack tport) :: Int
    when (not fe) initDB
    manager <- newManager conduitManagerSettings
    warp  prt $ App manager
