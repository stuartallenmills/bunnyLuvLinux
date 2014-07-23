{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Home where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.))
import Yesod.Default.Util
import Yesod.Auth
import Foundation

import Data.Text (Text, unpack)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Data.Time

--query:: IO ()
{-
queryAll = runSqlite "test5.db3" $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. RabbitName !=. val "")
     orderBy [asc (r ^. RabbitName)]
     return (r)
  return zipt
-}
query field value= runSqlite "test5.db3" $ do
  zipt<-select $ from $ \r->do
    where_ (r ^. field ==. val value)
    orderBy [asc (r ^. RabbitName)]
    return r
  return zipt

queryStatus status = runSqlite "test5.db3" $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. RabbitStatus ==. val status)
     orderBy [asc (r ^. RabbitName)]
     return (r)
  return zipt

querySource source = runSqlite "test5.db3" $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. RabbitSourceType ==. val source)
     orderBy [asc (r ^. RabbitName)]
     return (r)
  return zipt


-- menuWidget::Widget
-- menuWidget = $(widgetFileNoReload def "menu")

cssmenuWidget::Widget
cssmenuWidget = $(widgetFileNoReload def "cssmenu")

mainMenu::Widget
mainMenu = do
          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
          cssmenuWidget


  
getQueryR status  = do
     msg <- getMessage
     maid <- maybeAuthId
     zinc<- queryStatus status
     today<- liftIO $ getCurrentDay
     defaultLayout $ do
        setTitle "Rabbits"
        [whamlet|
         ^{headerLogWid maid}
         ^{mainMenu}

     $forall Entity rabbitid rabbit <- zinc
           ^{doRabbitRow  today rabbitid rabbit }
                |]

getSourceR source  = do
    msg <- getMessage
    maid <- maybeAuthId
    zinc<- querySource source
    today<- liftIO $ getCurrentDay
    defaultLayout $ do
        setTitle "Source"
        [whamlet|
         ^{headerLogWid maid}
         ^{mainMenu}
     $forall Entity rabbitid rabbit <- zinc
           ^{doRabbitRow today rabbitid rabbit }
                |]

doRabbitRow::Day->RabbitId->Rabbit->Widget
doRabbitRow today rabbitid rabbit = $(widgetFileNoReload def "rabRow") 

getTestR :: Handler Html
getTestR = do
  defaultLayout $ do
     setTitle "Test"
     [whamlet|
      ^{mainMenu}
      <div>This is a test of the something
            |]
  
getHomeR :: Handler Html
getHomeR = do
    msg <- getMessage
    maid <- maybeAuthId
    auth <- isAdmin
    let isAuth=(auth==Authorized)
    bl <-queryStatus "BunnyLuv"
    ad <-queryStatus "Adopted"
    di <-queryStatus "Died"
    eu <-queryStatus "Euthanized"
--    zinc <-queryAll
    today<- liftIO $ getCurrentDay
    let zinc = bl++ad++di++eu
    defaultLayout $ do
        setTitle "Rabbits"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
     -- <div class="title" style="width:95%; margin-left:1%; margin-right:1%;" >
     --  <div #headimg>
     --   <img src="http://localhost:3000/images/bunnyluv_img.gif" width="80px">
     --  <div #splash>  
     --   <h2> BunnyLuv Rabbit Database
        toWidget [julius| $( document ).ready(function(){
                             if (#{isAuth}) { 
                              $( "#cssmenu li:eq(1)" ).show(); }
                             else {
                              $( "#cssmenu li:eq(1)" ).hide(); }
                           });
                             |]
        [whamlet|
         ^{headerLogWid maid}
         ^{mainMenu}
         <div #rabbitContainer>
     $forall Entity rabbitid rabbit <- zinc
           ^{doRabbitRow today rabbitid rabbit }
                |]

