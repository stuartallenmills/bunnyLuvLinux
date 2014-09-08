{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.AdoptionReq where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Database.Persist.Sqlite ( runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import FormUtils

data TAdoptedReq = TAdoptedReq {
                  taDate::Day
                  ,taPerson::Person
                  ,taForm::Maybe FileInfo
                  } 


adoptedReqForm::Html-> MForm Handler (FormResult TAdoptedReq, Widget)
adoptedReqForm  extra = do
    local_time <- liftIO  getLocalTime
    let stime = showtime (localDay local_time)
    (adoptedDateRes, adoptedDateView)<-mreq textField "nope" (Just stime)
    (adoptedFirstNameRes, adoptedFirstNameView)<-mreq textField "nope" Nothing
    (adoptedLastNameRes, adoptedLastNameView)<-mreq textField "nope" Nothing
    (adoptedPhoneRes, adoptedPhoneView)<-mreq textField "nope" Nothing
    (adoptedMobileRes, adoptedMobileView)<-mopt textField "noop" Nothing
    (adoptedStreetRes, adoptedStreetView)<-mreq textField "nope" Nothing
    (adoptedAptRes, adoptedAptView)<-mopt textField "noop" Nothing
    (adoptedCityRes, adoptedCityView)<-mreq textField "nope" Nothing
    (adoptedStateRes,adoptedStateView)<-mreq textField "nope" Nothing
    (adoptedZipRes, adoptedZipView)<-mreq textField "nope" Nothing
    (adoptedEmailRes, adoptedEmailView)<-mopt textField "nope" Nothing
    (adoptedFormRes, adoptedFormView)<-mopt fileField "nope" Nothing
    let date = fmap (doparseTime.unpack) adoptedDateRes
    let adoptedRes = TAdoptedReq <$> date <*> (Person <$> adoptedFirstNameRes <*>
                        adoptedLastNameRes <*>  adoptedPhoneRes <*>
                         adoptedMobileRes <*> adoptedStreetRes <*> adoptedAptRes <*>
                         adoptedCityRes <*> adoptedStateRes <*> adoptedZipRes <*>
                         adoptedEmailRes) <*> adoptedFormRes

    let adoptwid = do
          toWidget
            [lucius|
                     #fadopted input {
                       display:inline;
                      }
                     #fadopted div {
                       float:left;
                       margin:5px;
                     }

                     #fadopted #add, #fadopted #phones, #fadopted #statezip {
                                margin-left:0;
                     }
                     #faDate, #faCity, #statezip, #phones,  #email {
                        width:90%;
                     }
                     #faState, #faHPhone {
                        width:50%;
                     }
                     #fadopted { width:100%;}
                     ##{fvId adoptedFirstNameView} {width:20em}
                     ##{fvId adoptedLastNameView} {width:20em}
                     ##{fvId adoptedStreetView} {width:25em}
                     ##{fvId adoptedCityView} {width:25em}
                     ##{fvId adoptedEmailView} {width:25em;}
                     ##{fvId adoptedPhoneView} {width:8em;}
                     ##{fvId adoptedMobileView} {width:8em;}
             |]

          [whamlet|
                    #{extra}
                   <div #fadopted>
                     <div #faDate .blDate>
                      Date: ^{fvInput adoptedDateView}
                     <div #faPersonFirst>
                      First Name : ^{fvInput adoptedFirstNameView} 
                     <div #faPersonLast>
                      Last Name :   ^{fvInput adoptedLastNameView}
                     <div #add>
                      <div #faStreet>
                       Street: ^{fvInput adoptedStreetView}
                      <div #apt>
                         Apt: ^{fvInput adoptedAptView}
                     <div #faCity>
                      City : ^{fvInput adoptedCityView}
                     <div #statezip>
                      <div #faState>
                        State : ^{fvInput adoptedStateView}
                      <div #faZip>
                        Zip : ^{fvInput adoptedZipView}
                     <div #phones>
                      <div #faHPhone>
                       Home Phone:  ^{fvInput adoptedPhoneView}
                      <div #faMPhone>
                       Mobile : ^{fvInput adoptedMobileView}
                     <div #email>
                       Email: ^{fvInput adoptedEmailView}
                     <div #file>
                       Adoption Form File : ^{fvInput adoptedFormView}
                 <br>
                 <div #submit>
                   <input type=submit value="submit">
        |]

    return (adoptedRes, adoptwid)




getAdoptReqR ::Handler Html
getAdoptReqR  = do

    (formWidget, enctype) <- generateFormPost adoptedReqForm
    let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                  <b> New Adoption Request </b>
                <div #adoptCan style="float:right; display:inline;">
                  <div .cancelBut #adoptEdCan style="display:inline; float:right;">cancel
           |]
    let form = [whamlet|         
              <form method=post action=@{AdoptReqR} enctype=#{enctype}>
                <div #pers style="width:100%; float:left;">
                 ^{formWidget}
         |]
    baseForm "Adoption" menu form
  

postAdoptReqR::Handler Html
postAdoptReqR  = do
  ((result, _), _) <-runFormPost adoptedReqForm 

  case result of
    FormSuccess (TAdoptedReq date person (Just form)) -> do
      filename <- writeToServer form
      runDB $ do
        personId <- insert person
        _ <-insert  (AdoptRequest date personId  Nothing  (Just (pack filename)))
        return ()
    FormSuccess (TAdoptedReq date person Nothing) -> do
      runDB $ do
        personId <- insert person
        _ <-insert  (AdoptRequest date personId  Nothing  Nothing)
        return ()
    _ -> return ()
  redirect ViewAdoptForms

    
