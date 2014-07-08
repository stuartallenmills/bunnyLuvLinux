{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.VetVisit where

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

vetVisitForm:: RabbitId->Html-> MForm Handler (FormResult VetVisit, Widget)
vetVisitForm rabid extra = do
    (vvDateRes, vvDateView)<-mreq textField "nope" Nothing
    (vvVetRes, vvVetView)<-mreq (selectFieldList vets) "nope" Nothing
    (vvProblemRes, vvProblemView)<-mreq textField "nopte" Nothing
    (vvProceduresRes, vvProceduresView)<-mreq textField "n" Nothing
    (vvNotesRes, vvNotesView)<-mreq textField "n" Nothing
    (vvSpayRes, vvSpayView)<-mreq boolField "n" Nothing
    (vvCostRes, vvCostView)<-mreq doubleField "n" Nothing
    let date = text2date vvDateRes
    let vetvisitRes = VetVisit rabid <$> vvVetRes <*> date <*> vvProblemRes <*> vvProceduresRes <*> vvNotesRes <*> vvSpayRes <*> vvCostRes 
    let vwidget = do 
         toWidget
             [lucius|
              #fvVisit div {
                 float:float;
                 margin:1%;
                }
              #fvDate {
                width:30%;
              }
              #fvVet {
                width:40%;
              }
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
              #fvSpay {width:30%}
              #fvCost { width:30%}
              #fvProblem, #fvProcedures, vfNotes {width:98%}
                 ##{fvId vvDateView} {display:inline}
                 ##{fvId vvVetView} {display:inline}
                 ##{fvId vvCostView} {display:inline; width:15 em;}    
                 ##{fvId vvSpayView}
                          {display:inline;}
                 ##{fvId vvProblemView} {
                     width:30em;
                 }
                 ##{fvId vvProceduresView} { width:30em; }
                 ##{fvId vvNotesView} {width:30em;}
                 
                 |]
         [whamlet|
            #{extra}
            <div #fvVisit>
             <div #fvTitle>
                 VetVisit
             <div #fvDate>
               Date:  ^{fvInput vvDateView}
             <div #fvVet>
               Vet:   ^{fvInput vvVetView}
             <div #fvSpay>
              Spay:   ^{fvInput vvSpayView}
             <div #fvCost>
               Cost:  ^{fvInput vvCostView}
             <div #vfProblem>
               Problem:   ^{fvInput vvProblemView}
             <div #vfProcedures>
              Procedures:   ^{fvInput vvProceduresView}
             <div #fvNotes>
               Notes:  ^{fvInput vvNotesView}
             <div .cancelBut>
                   <a href=@{HomeR}>cancel
             <input type=submit value="submit">
            |]
    return (vetvisitRes, vwidget)
        
getVetVisitR ::RabbitId->Handler Html
getVetVisitR rabid = do
    (formWidget, enctype) <- generateFormPost (vetVisitForm rabid)
    defaultLayout $ do
         setTitle "Vet Visit"
         [whamlet|
             ^{headerWidget}
              <div #addCance style="text-align:center">
                 <b> Vet Visit
              <form method=post action=@{VetVisitR rabid} enctype=#{enctype}>
                 ^{formWidget}
          |]
  

postVetPostR::RabbitId->Handler Html
postVetPostR  rabID = do
  (((result), _), _) <-runFormPost (vetVisitForm rabID)

  case result of
    FormSuccess vetVisit -> do
      runSqlite "test5.db3" $ do
        _ <-insert  vetVisit
        return ()
    _ -> return ()

  redirect HomeR
