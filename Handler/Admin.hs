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

backupForm :: Html -> MForm Handler  (FormResult (FileInfo), Widget)
backupForm  = renderDivs $ fileAFormReq "backupe"
{-
getBackupR::Handler Html
getBackupR  = do
  ((_, widget), enctype) <-runFormPost backupForm
  let menu = [whamlet| <b> Upload Rabbit Image 
                <div #wellCan style="float:right; display:inline;">
                  <div .cancelBut #wellEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
                     |]
  let form = [whamlet|            
                <form method=post enctype=#{enctype}>
                  ^{widget}
                 <input .btn type=submit value="Upload">
      |]
  baseForm "Upload Image" menu form
-}

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
 impath <- liftIO getImagePath
 let imgpath = unpack impath

 (formWidget, enctype) <- generateFormPost addUsrForm
 defaultLayout $ do
         setTitle "Add User"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerLogWid imgpath maid}
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
      runSqlite bunnyLuvDB $ do
        insert usr
        return ()
    _ -> return ()
  redirect HomeR

p2 = FieldSettings "pass2" (Just "Verify Password") (Just "pass2") Nothing []
p1 = FieldSettings "pass1" (Just "Enter Password") (Just "pass1") Nothing []

changePassForm::Text->Html->MForm Handler (FormResult Usr, Widget) 
changePassForm uname extra = do
  (usrNameRes, usrNameView)<- mreq textField "hello" (Just uname)
  (usrPassRes, usrPassView) <- mreq passwordField p1 Nothing
  (_ , usrPassView2) <- mreq passwordField p2 Nothing
  let usrRes = Usr <$> usrNameRes <*> usrPassRes
  
  let usrW=do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
        toWidget [julius| $( function () {
                          $( "#pass2" ).keyup (function(){
                              if ( $( "#pass2" ).val() == $( "#pass1" ).val() ) {
                                $( "#match").text("Match!");
                                $( "#chngpass" ).prop( "disabled", false);
                              } else {
                                $( "#match" ).text("Passwords don't match");
                                $( "#chngpass" ).prop( "disabled", true );
                              }
                            });
                           });
                   |]
        [whamlet|
          #{extra}
          Name: ^{fvInput usrNameView}
          Password ^{fvInput usrPassView}
          Re-enter Password ^{fvInput usrPassView2}
          <br>
          <div #match>
          <input #chngpass type=submit value="submit">
          |]
  return (usrRes, usrW)

getChangePassR::Handler Html  
getChangePassR = do
 Just maid <- maybeAuthId
 impath <- liftIO getImagePath
 let imgpath = unpack impath
 (formWidget, enctype) <- generateFormPost (changePassForm maid)
 defaultLayout $ do
         setTitle "Change Password"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerLogWid imgpath (Just maid)}
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
      runSqlite bunnyLuvDB $ do
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
 impath <- liftIO getImagePath
 let imgpath = unpack impath
 (formWidget, enctype) <- generateFormPost deleteUsrForm
 defaultLayout $ do
         setTitle "Delete User"
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerLogWid imgpath (Just maid)}
              <div #addCance style="text-align:left; margin-top:5px; margin-bottom:8px;">
                <b> Delete User
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
      runSqlite bunnyLuvDB $ do
        delete $ from $ \user -> do
          where_ (user ^. UsrLoginName ==. val usrname)
        return ()
    _ -> return ()
  redirect HomeR
