{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.AdoptionForm where

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
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import FormUtils

data AdoptTest = AdoptTest {
                      addate::Text
                      ,adownRab::Bool
                      ,addesc::Maybe Textarea
                      ,adcompan:: Bool
                      ,adDiet::Maybe Textarea
                      ,adReason::Textarea
                      ,adResearch::Textarea
                      ,adRescue::Textarea
                      ,adAllergy::Bool
                      ,adCareGiver::Text
                      ,adOwn::Textarea
                      ,adProof::Textarea
                      ,adEnclosure::Textarea
                      ,adExercise::Textarea
                      ,adVacation::Textarea
                      ,adVet::Textarea
                      ,adPets::Textarea
                       } deriving Show

aMaster::Html->MForm Handler (FormResult (Person, AdoptTest), Widget)
aMaster extra = do
  (res1, p1Widget) <-  adoptionForm 
  (res2, perWidget) <-  personForm 
  let wid = $(widgetFileNoReload def "AdoptionForm")
  let ares = (,) <$> res2 <*> res1
  return (ares, wid)

adoptionForm::MForm Handler (FormResult AdoptTest, Widget)
adoptionForm= do
  (dateRes, dateView)<- mreq textField "nope" Nothing
  (ownRabRes, ownRabView)<-mreq boolField "nope" Nothing
  (ownRabDescRes, ownRabDescView)<-mopt textareaField "nope" Nothing
  (companionRes, companionView)<-mreq boolField "nop" Nothing
  (dietRes, dietView)<-mopt textareaField "nope" Nothing
  (reasonRes, reasonView)<-mreq textareaField "nope" Nothing
  (researchRes, researchView)<-mreq textareaField "nope" Nothing
  (rescueRes, rescueView)<-mreq textareaField "nope" Nothing
  (allergyRes, allergyView)<-mreq boolField "nope" Nothing
  (careRes, careView)<-mreq textField "nope" Nothing
  (ownRes, ownView)<-mreq textareaField "nope" Nothing
  (proofRes, proofView)<-mreq textareaField "nope" Nothing
  (enclosureRes, enclosureView)<-mreq textareaField "nope" Nothing
  (exerciseRes, exerciseView)<-mreq textareaField "nope" Nothing
  (vacationRes, vacationView)<-mreq textareaField "nope" Nothing
  (vetRes, vetView)<-mreq textareaField "nope" Nothing
  (petsRes, petsView)<-mreq textareaField "nope" Nothing
  
  
  
  let wid1 = $(widgetFileNoReload def "AFormP1")
  let wid2 = $(widgetFileNoReload def "AFormP2");
  let wid3 = $(widgetFileNoReload def "AFormP3");
  let wid = do
         wid1
         wid2
         wid3
         toWidget [lucius|
                   ##{fvId careView} {
                       width:25em;
                       margin:3px;
                      }
                    ##{fvId petsView} {
                        height:12em;
                      }
         |]
  let res = AdoptTest <$> dateRes <*> ownRabRes <*> ownRabDescRes <*>companionRes <*>
                        dietRes <*> reasonRes <*> researchRes <*> rescueRes <*>
                        allergyRes <*> careRes <*> ownRes <*> proofRes <*> enclosureRes <*>
                        exerciseRes <*> vacationRes <*> vetRes <*> petsRes
  return (res, wid)

getAdoptionFormR::Handler Html
getAdoptionFormR = do
  (masterWidget, enctype)<-generateFormPost aMaster
  let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">

                <b> Adoption Form
                <div #vvCan style="float:right; display:inline;">
                  <div .cancelBut #vvEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
               |]
  let form = [whamlet|<form #adoptionFormForm method=post action=@{AdoptionFormR} enctype=#{enctype}>
                 ^{masterWidget}
               |]
  baseForm "Adoption Form" menu form

postAdoptionFormR::Handler Html
postAdoptionFormR = do
  ((perResf, _), _)<-runFormPost aMaster
  case perResf of
    FormSuccess (person, atest) -> do
                       defaultLayout  [whamlet|
                                     <div> Success
                                                #{personFirstName person}
                                         <div>
                                             #{adReason atest}

 
                                              |]
    _ -> do
           msg<-getMessage
           defaultLayout [whamlet| Error in Posting Form  
                          $maybe ms <-msg
                              ms
                      |]
