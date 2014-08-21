{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.DailyReport where

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




    
dailyReportForm::(Maybe Text)->Html-> MForm Handler (FormResult DailyReport, Widget)
dailyReportForm user  extra = do
    local_time <- liftIO  getLocalTime
    let stime = showtime (localDay local_time)
    (drDateRes, drDateView)<-mreq textField "nope" (Just stime)
    (drReportRes, drReportView)<-mreq textareaField "nope" Nothing
    (drResponsibleRes, drResponsibleView)<-mreq textField "nope" user
    
    let date = fmap (doparseTime.unpack)drDateRes
    let drRes = DailyReport  <$> date <*> drResponsibleRes <*> drReportRes
    let twid = do
          [whamlet| #{extra}
                      <div #drRow style="width:100%;" >
                       <div #drUsr, style="float:left;">
                          ^{fvInput drResponsibleView}
                       <div #drDate, style="float:right;">
                          ^{fvInput drDateView}
                      <div #drRow style="width:100%; float:left;">
                       <div #drHead, style="width:100%;">
                           Report:
                       <div #drRep>
                         ^{fvInput drReportView}
           |]
          toWidget [lucius|
              ##{fvId drReportView} {
                        width:35em;
                        height:25em;
                  }
                 

    |]
                       
    return (drRes, twid)
    

getDailyR::Handler Html
getDailyR   = do
    maid <- maybeAuthId
    (drWidget, enctype) <-generateFormPost (dailyReportForm maid )
    let menu = [whamlet|
               <div #addCance style="float:inherit; text-align:left; margin:10px;">
                <b> Daily Report
                <div #drCan style="float:right; display:inline;">
                  <div .cancelBut #drgCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
                |]
    let form = [whamlet| 
              <form method=post action=@{DailyR } enctype=#{enctype}>
                 ^{drWidget}
                <input  type=submit value="submit">
                 
            |]

    baseForm "New Daily Report" menu form



postDailyR::Handler Html
postDailyR  = do
  maid <-maybeAuthId
  ((result, _), _) <-runFormPost (dailyReportForm maid )
  case result of
    FormSuccess dr -> 
      runDB $ do
        insert dr
        return ()
    _ ->return ()
  redirect HomeR 
