{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Admin where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update, insert, delete)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import qualified Data.Map as Map

addUsrForm::Html->MForm Handler (FormResult Usr, Widget) 
addUsrForm extra = do
  (usrNameRes, usrNameView)<- mreq textField "hello" Nothing
  (usrPassRes, usrPassView) <- mreq passwordField "hello" Nothing
  let usrRes = Usr <$> usrNameRes <*> usrPassRes
  let usrW=
        [whamlet|
          #{extra}
          Name: ^{fvInput usrNameView}
          Password ^{fvInput usrPassView}
          <input type=submit value="submit">
          |]
  return (usrRes, usrW)

getAddUR::Handler Html  
getAddUR = do
 maid <- maybeAuthId
 (formWidget, enctype) <- generateFormPost addUsrForm
 defaultLayout $ do
         setTitle "Add Usr"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerLogWid maid}
              <div #addCance style="text-align:left; margin-top:5px; margin-bottom:8px;">
                <b> Add New User
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
              <form method=post action=@{AddUR} enctype=#{enctype}>
                 ^{formWidget}
                 |]
           
postAddUR::Handler Html
postAddUR = do
  ((result, _), _) <-runFormPost addUsrForm 
  case result of
    FormSuccess usr -> 
      runSqlite "test5.db3" $ do
        insert usr
        return ()
    _ -> return ()
  redirect HomeR


changePassForm::Text->Html->MForm Handler (FormResult Usr, Widget) 
changePassForm uname extra = do
  (usrNameRes, usrNameView)<- mreq textField "hello" (Just uname)
  (usrPassRes, usrPassView) <- mreq passwordField "hello" Nothing
  let usrRes = Usr <$> usrNameRes <*> usrPassRes
  let usrW=
        [whamlet|
          #{extra}
          Name: ^{fvInput usrNameView}
          Password ^{fvInput usrPassView}
          <input type=submit value="submit">
          |]
  return (usrRes, usrW)

getChangePassR::Handler Html  
getChangePassR = do
 Just maid <- maybeAuthId
 (formWidget, enctype) <- generateFormPost (changePassForm maid)
 defaultLayout $ do
         setTitle "Add Usr"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerLogWid (Just maid)}
              <div #addCance style="text-align:left; margin-top:5px; margin-bottom:8px;">
                <b> Change Password
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
              <form method=post action=@{ChangePassR} enctype=#{enctype}>
                 ^{formWidget}
                 |]

postChangePassR::Handler Html
postChangePassR = do
  ((result, _), _) <-runFormPost addUsrForm 
  case result of
    FormSuccess (Usr nme pss) -> 
      runSqlite "test5.db3" $ do
        update $ \user -> do
          set user [UsrPassword =. val pss]
          where_ (user ^. UsrLoginName ==. val nme)
        return ()
    _ -> return ()
  redirect HomeR
 
           

deleteUsrForm::Html->MForm Handler (FormResult Text, Widget) 
deleteUsrForm  extra = do
  usrMap <- liftIO $ getUsrs
  let ulist = Map.keys usrMap
  let usrList = map (\x->(x,x)) ulist
  (usrRes, usrNameView)<- mreq (selectFieldList usrList) "hello" Nothing
  let usrW= do
        [whamlet|
          #{extra}
          Select User: ^{fvInput usrNameView}
          <input type=submit value="submit">
          |]
        toWidget [lucius| 
                     ##{fvId usrNameView} {
                            width:20em;
                            margin:10px;
                        }
                |]
  return (usrRes, usrW)

getDeleteUR::Handler Html  
getDeleteUR = do
 Just maid <- maybeAuthId
 (formWidget, enctype) <- generateFormPost deleteUsrForm
 defaultLayout $ do
         setTitle "Delete Usr"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerLogWid (Just maid)}
              <div #addCance style="text-align:left; margin-top:5px; margin-bottom:8px;">
                <b> Change Password
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
              <form method=post action=@{DeleteUR} enctype=#{enctype}>
                 ^{formWidget}
                 |]

postDeleteUR::Handler Html
postDeleteUR = do
  ((result, _), _) <-runFormPost deleteUsrForm 
  case result of
    FormSuccess usrname -> 
      runSqlite "test5.db3" $ do
        delete $ from $ \user -> do
          where_ (user ^. UsrLoginName ==. val usrname)
        return ()
    _ -> return ()
  redirect HomeR
