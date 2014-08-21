{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Wellness where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.))
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






    
wellnessForm::(Maybe Text)->RabbitId->Html-> MForm Handler (FormResult Wellness, Widget)
wellnessForm user rabID extra = do
    local_time <- liftIO  getLocalTime
    let stime = showtime (localDay local_time)
    (wellDateRes, wellDateView)<-mreq textField "nope" (Just stime)
    (wellLbsRes, wellLbsView)<-mreq intField "nope" Nothing
    (wellOzRes, wellOzView)<-mreq intField "nope" Nothing
    (wellTempRes, wellTempView)<-mopt doubleField "nope" Nothing
    (wellNoteRes, wellNotesView)<-mreq textField "nope" Nothing
    (wellGroomedRes, wellGroomedView)<-mreq boolField "nope" (Just False)
    (wellTreatmentRes, wellTreatmentView)<-mreq textareaField "nope" Nothing
    (wellResponsibleRes, wellResponsibleView)<-mreq textField "nope" user
    
    let date = fmap (doparseTime.unpack) wellDateRes
    let wellnessRes = Wellness rabID <$> date <*> wellGroomedRes <*>
                        wellTempRes <*> (Weight <$> wellLbsRes <*> wellOzRes) <*>
                         wellNoteRes <*> wellTreatmentRes <*> wellResponsibleRes
    let twid = $(widgetFileNoReload def "wellness")
    return (wellnessRes, twid)
    

getWellnessR::RabbitId->Handler Html
getWellnessR rabID  = do
    maid <- maybeAuthId
    Just rabbit <-runDB  $ do
                  rabt<- get rabID
                  return rabt
    (wellnessWidget, enctype) <-generateFormPost (wellnessForm maid rabID)
    let menu = [whamlet|
               <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Wellness Report for &nbsp;  #{rabbitName rabbit}
                <div #wellCan style="float:right; display:inline;">
                  <div .cancelBut #wellEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabID }> cancel </a>
                |]
    let form = [whamlet| 
              <form method=post action=@{WellnessR rabID} enctype=#{enctype}>
                 ^{wellnessWidget}
            |]

    baseForm "Wellness Report" menu form



postWellnessR::RabbitId->Handler Html
postWellnessR rabID = do
  maid <-maybeAuthId
  (((result), _), _) <-runFormPost (wellnessForm maid rabID)
  case result of
    FormSuccess wup -> do
      runDB $ do
        insert wup
        return ()
    _ -> return ()
  redirect (ViewR rabID)
