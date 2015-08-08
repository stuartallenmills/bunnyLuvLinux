{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM

import Yesod
import System.Directory
import Control.Monad
import           Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import           Yesod.Auth
import Yesod.Static
import Dispatch ()
import Foundation
import Data.Text (unpack)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Logger 

makeusers = do
   runSqlite usrsDB $ do
       runMigration migrateUsr
       insert $ Usr "sharon" "bunnyluv"
       insert $ Usr "stuart" "jrr1jrr1"
       insert $ Usr "demo" "demo"
   return ()

main :: IO ()
main = do
    static@(Static settings)<- static "static"
    fe<-doesFileExist (unpack bunnyLuvDB)
    tport <- getPort
    let prt = read (unpack tport) :: Int
  {-}  when (not fe) initDB -}
    areusrs <- doesFileExist (unpack usrsDB)
    unless areusrs makeusers
    
    manager <- newManager conduitManagerSettings
    pool1 <-  runStderrLoggingT $ createSqlitePool bunnyLuvDB 10
    pool2 <-  runStderrLoggingT $  createSqlitePool demoDB 10
    runStderrLoggingT $ withSqlitePool bunnyLuvDB 10 $ \pool-> liftIO $ do
      runSqlPersistMPool (runMigration migrateAll) pool
    runStderrLoggingT $ withSqlitePool demoDB 10 $ \pool2->  liftIO $ do
      runSqlPersistMPool (runMigration migrateAll) pool2
    warp  prt $ App  manager pool1 pool2 static
