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
import Yesod hiding ((!=.), (==.))
import Yesod.Default.Util
import Foundation

import Data.Text (Text, unpack)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative





headerWidget::Widget
headerWidget = $(widgetFileNoReload def "header")


adoptedForm::RabbitId->Html-> MForm Handler (FormResult Adopted, Widget)
adoptedForm rabID extra = do
    (adoptedDateRes, adoptedDateView)<-mreq textField "nope" Nothing
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
                     <div #faPersonFirst>
                      First Name : ^{fvInput adoptedFirstNameView} 
                     <div #faPersonLast>
                      Last Name :   ^{fvInput adoptedLastNameView}
                     <div #faPhone>
                      Phone:  ^{fvInput adoptedPhoneView}
                     <div #faStreet>
                      Street: ^{fvInput adoptedStreetView}
                     <div #faCity>
                      City : ^{fvInput adoptedCityView}
                     <div #faState>
                      State : ^{fvInput adoptedStateView}
                     <div #faZip>
                      Zip : ^{fvInput adoptedZipView}
                    <div .cancelBut>
                     <a href=@{HomeR}>cancel
                    <input type=submit value="submit">
        |]
{-
    let wlucius =  toWidget
           [lucius|
                   .cancelBut {
                     background: none repeat scroll 0 0 #09c;
                     border: 1pt solid #999;
                     border-radius: 5pt;
                     color: #fff;
                     float: right;
                     font-size: 80%;
                     height: 19px;
                     padding: 0 13px 0 0;
                     transform: translateY(-5px);
                     width: 50px;
                     }
                                 /* Change color on mouseover */
                     .cancelBut:hover {
                                  background:#fff;
                                  color:#09c;
                     }

                     .cancelBut a {
                            text-decoration:none;
                            color: #fff;
                            float:right;
                     }

                     .cancelBut a:hover {
                             color:#09c;
                    }
                 |]
    
 -}
    return (adoptedRes, adoptwid)




getAdoptedR ::RabbitId->Handler Html
getAdoptedR rabid = do
    (formWidget, enctype) <- generateFormPost (adoptedForm rabid)
    defaultLayout $ do
         setTitle "Vet Visit"
         [whamlet|
             ^{headerWidget}
              <div #addCance style="text-align:center">
                 <b> Adopted
              <form method=post action=@{VetPostR rabid} enctype=#{enctype}>
                 ^{formWidget}
          |]
  

postAdoptedR::RabbitId->Handler Html
postAdoptedR  rabID = do
  (((result), _), _) <-runFormPost (adoptedForm rabID)

  case result of
    FormSuccess adopted -> do
      runSqlite "test5.db3" $ do
        _ <-insert  adopted
        return ()
    _ -> return ()

  redirect HomeR

    
