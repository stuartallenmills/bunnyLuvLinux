{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Vets where


import Conduit
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import Text.Julius
import FormUtils
import Utils


addVetForm::Html -> MForm Handler (FormResult Vet, Widget)
addVetForm extra = do
  (practiceRes, practiceView)<-mreq textField "nope" Nothing
  (doctorRes, doctorView)<-mreq textField "nope" Nothing
  (phoneRes, phoneView)<-mreq textField "nope" Nothing
  let vetRes = Vet <$> practiceRes <*> doctorRes <*> phoneRes
  let wid = do
        [whamlet| #{extra}
         <div #vetForm>
          <div #prac>
            Clinic Name: ^{fvInput practiceView}
          <div #vet>
            Veterarian: ^{fvInput doctorView}
          <div #aphone>
            Phone: ^{fvInput phoneView}
         <input type="submit" value="Add Vet">
         |]
        toWidget [lucius|
              #vetForm input {
                display:inline;
               }
              ##{fvId practiceView} {
                  width:20em;
                }
              ##{fvId doctorView} {
                  width:20em;
                }
              ##{fvId phoneView} {
                  width:10em;
                }
            |]
  return (vetRes, wid)

getAddVetR::Handler Html
getAddVetR = do
  (formWidget, enctype)<-generateFormPost addVetForm
  let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Add Vet
                <div .cancelBut #rabEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
                |]
  let form = [whamlet|
               <form method=post action=@{AddVetR} enctype=#{enctype}>
                   ^{formWidget}
                |]
  baseForm "Add Vet" menu form

postAddVetR::Handler Html
postAddVetR = do
  ((result,_), _) <- runFormPost addVetForm
  case result of
    FormSuccess vet-> do
      runDB $ insert vet
      redirect HomeR
    _->defaultLayout $ [whamlet| Form Error! |]
