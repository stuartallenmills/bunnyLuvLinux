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
import Data.Time.Calendar
import FormUtils

data AdoptTest2 = AdoptTest2 {
                    ad2Date::Day
                    ,ad2info::AdoptInfo
                     } deriving Show
                                
data AdoptTest = AdoptTest {
                      adownRab::Bool
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
                      ,adWhere::Textarea
                      ,adVetcare::Textarea
                      ,adApt::Textarea
                      ,adRoom::Textarea
                      ,adOther::Textarea
                      ,adSeparate::Textarea
                      ,adChange::Textarea
                      ,adPermission::Bool
                      ,adFind::Textarea
                       } deriving Show

aMaster::Html->MForm Handler (FormResult (Person, AdoptTest2), Widget)
aMaster extra = do
  (res1, p1Widget) <-  adoptionForm 
  (res2, perWidget) <-  (personForm Nothing)
  let wid = $(widgetFileNoReload def "AdoptionForm")
  let ares = (,) <$> res2 <*> res1
  return (ares, wid)

adoptionForm::MForm Handler (FormResult AdoptTest2, Widget)
adoptionForm= do
  local_time <- liftIO getLocalTime
  let today = localDay local_time
  let strTime = showtime today
  (dateRes, dateView)<- mreq textField "nope" (Just strTime)
  (ownRabRes, ownRabView)<-mreq boolField "nope" Nothing
  (ownRabDescRes, ownRabDescView)<-mopt textareaField "nope" Nothing
  (companionRes, companionView)<-mreq boolField "nop" Nothing
  (dietRes, dietView)<-mopt textareaField "nope" Nothing
  (reasonRes, reasonView)<-mreq textareaField "nope" Nothing
  (howlongRes, howlongView)<-mreq textareaField "nope" Nothing
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
  (vetcareRes, vetcareView)<-mreq textareaField "nope" Nothing
  (aptRes, aptView)<-mreq textareaField "nope" Nothing
  (roomRes, roomView)<-mreq textareaField "nope" Nothing
  (otherRes, otherView)<-mreq textareaField "nope" Nothing
  (separateRes, separateView)<- mreq textareaField "nope" Nothing
  (changeRes, changeView)<-mreq textareaField "nope" Nothing
  (permissionRes, permissionView)<-mreq boolField "nope" Nothing
  (findRes, findView)<-mreq textareaField "nope" Nothing
  
  let wid1 = $(widgetFileNoReload def "AFormP1")
  let wid2 = $(widgetFileNoReload def "AFormP2");
  let wid3 = $(widgetFileNoReload def "AFormP3");
  let wid4 = $(widgetFileNoReload def "AFormP4");
  let wid5 = $(widgetFileNoReload def "AFormP5");
  let wid = do
         wid1
         wid2
         wid3
         wid4
         wid5
         toWidget [lucius|
                   ##{fvId careView} {
                       width:25em;
                       margin:3px;
                      }
                    ##{fvId petsView} {
                        height:12em;
                      }
         |]
  let date = text2date dateRes
  let ainfo = AdoptInfo  <$> ownRabRes <*> ownRabDescRes <*>companionRes <*>
                        dietRes <*> reasonRes <*> howlongRes <*> researchRes <*> rescueRes <*>
                        allergyRes <*> careRes <*> ownRes <*> proofRes <*> enclosureRes <*>
                        exerciseRes <*> vacationRes <*> vetRes <*> petsRes <*>
                         vetcareRes <*> aptRes <*> roomRes <*> otherRes <*>
                        separateRes <*> changeRes <*> permissionRes <*> findRes
  let res =AdoptTest2 <$> date <*> ainfo
  return (res, wid)

adForWid masterWidget enctype = do 
  [whamlet|      
          <form #adoptionFormForm method=post action=@{AdoptionFormR} enctype=#{enctype}>
                 ^{masterWidget} |]
  toWidget [lucius|
            #adoptAp {
              border-top:2px solid red;
             }
            #adoptAp a {
              color:red;
             }
           |]
                                

nullWid = [whamlet| <div #null> |]

getAdoptionFormR::Handler Html
getAdoptionFormR = do
  (masterWidget, enctype)<-generateFormPost aMaster
  baseAdoption "Adoption Application" nullWid (adForWid masterWidget enctype)

thanksWid =  [whamlet|
              <div> 
              Thanks for applying!
              <br>
              We will give you call shortly to discuss your application.
  |]
postAdoptionFormR::Handler Html
postAdoptionFormR = do
  ((perResf, _), _)<-runFormPost aMaster
  case perResf of
    FormSuccess (person, atest) -> do
                       runDB $ do
                        personID<-insert person
                        let tareq = AdoptRequest (ad2Date atest)  personID (Just (ad2info atest)) Nothing
                        reqID<- insert tareq
                        return ()
                       baseAdoption "Thanks!" thanksWid nullWid 
                         
    _ -> do
           msg<-getMessage
           defaultLayout [whamlet| Error in Posting Form  
                          $maybe ms <-msg
                              ms
                      |]
