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
import Database.Persist
import Database.Persist.Sqlite

makeusers = do
   runSqlite usrsDB $ do
       runMigration migrateUsr
       insert $ Usr "sharon" "bunnyluv"
       insert $ Usr "stuart" "jrr1jrr1"
   return ()

main :: IO ()
main = do
    fe<-doesFileExist (unpack bunnyLuvDB)
    tport <- getPort
    let prt = read (unpack tport) :: Int
    when (not fe) initDB
    areusrs <- doesFileExist (unpack usrsDB)
    unless areusrs makeusers
    
    manager <- newManager conduitManagerSettings
    withSqlitePool bunnyLuvDB 10 $ \pool-> do
      runSqlPersistMPool (runMigration migrateAll) pool
      warp  prt $ App  manager pool
