{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Adopted where

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
import Data.Text (Text, unpack)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import FormUtils



adoptedForm::RabbitId->Html-> MForm Handler (FormResult Adopted, Widget)
adoptedForm rabID extra = do
    local_time <- liftIO $ getLocalTime
    let stime = showtime (localDay local_time)
    (adoptedDateRes, adoptedDateView)<-mreq textField "nope" (Just stime)
    (adoptedFirstNameRes, adoptedFirstNameView)<-mreq textField "nope" Nothing
    (adoptedLastNameRes, adoptedLastNameView)<-mreq textField "nope" Nothing
    (adpotedPhoneRes, adoptedPhoneView)<-mreq textField "nope" Nothing
    (adoptedStreetRes, adoptedStreetView)<-mreq textField "nope" Nothing
    (adoptedCityRes, adoptedCityView)<-mreq textField "nope" Nothing
    (adoptedStateRes,adoptedStateView)<-mreq textField "nope" Nothing
    (adoptedZipRes, adoptedZipView)<-mreq textField "nope" Nothing
    let date = fmap (doparseTime.unpack) adoptedDateRes
    let adoptedRes = Adopted rabID <$> date <*> (Person <$> adoptedFirstNameRes <*>
                        adoptedLastNameRes <*>  adpotedPhoneRes <*> adoptedStreetRes <*>
                         adoptedCityRes <*> adoptedStateRes <*> adoptedZipRes)

    let adoptwid = do
          toWidget
            [lucius|
                     #fadopted { width:100%;}
                     ##{fvId adoptedFirstNameView} {width:20em}
                     ##{fvId adoptedLastNameView} {width:20em}
                     ##{fvId adoptedStreetView} {width:25em}
                     ##{fvId adoptedCityView} {width:25em}
             |]

          [whamlet|
                    #{extra}
                   <div #fadopted>
                     <div #faDate>
                      Date: ^{fvInput adoptedDateView}
                     <div #faPhone>
                      Phone:  ^{fvInput adoptedPhoneView}
                     <div #faPersonFirst>
                      First Name : ^{fvInput adoptedFirstNameView} 
                     <div #faPersonLast>
                      Last Name :   ^{fvInput adoptedLastNameView}
                     <div #faStreet>
                      Street: ^{fvInput adoptedStreetView}
                     <div #faCity>
                      City : ^{fvInput adoptedCityView}
                     <div #faState>
                      State : ^{fvInput adoptedStateView}
                     <div #faZip>
                      Zip : ^{fvInput adoptedZipView}
                    <input type=submit value="submit">
        |]

    return (adoptedRes, adoptwid)




getAdoptedR ::RabbitId->Handler Html
getAdoptedR rabid = do
    Just rabbit <-runSqlite bunnyLuvDB  $ do
                  rabt<- get rabid
                  return rabt
    (formWidget, enctype) <- generateFormPost (adoptedForm rabid)
    let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                 <b> Adoption for &nbsp; #{rabbitName rabbit}
                <div #adoptCan style="float:right; display:inline;">
                  <div .cancelBut #adoptEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabid }> cancel </a>
          |]
    let form = [whamlet|         
              <form method=post action=@{AdoptedR rabid} enctype=#{enctype}>
                 ^{formWidget}
         |]
    baseForm "Adoption" menu form
  

postAdoptedR::RabbitId->Handler Html
postAdoptedR  rabID = do
  ((result, _), _) <-runFormPost (adoptedForm rabID)

  case result of
    FormSuccess adopted -> do
      runSqlite bunnyLuvDB $ do
        _ <-insert  adopted
        update $ \p -> do
          set p [RabbitStatus =. val "Adopted", RabbitStatusDate =. val (showtime (adoptedDate adopted))]
          where_ (p ^. RabbitId ==. val rabID)
          return ()
    _ -> return ()

  redirect (ViewR rabID)

    
