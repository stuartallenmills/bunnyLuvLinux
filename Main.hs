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
    when (not fe) initDB
    manager <- newManager conduitManagerSettings
    warp 3050 $ App manager
