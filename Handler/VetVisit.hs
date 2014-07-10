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
    (vvSpayRes, vvSpayView)<-mreq (selectFieldList procedures) "n" Nothing
    (vvCostRes, vvCostView)<-mopt doubleField "n" Nothing
    let date = text2date vvDateRes
    let vetvisitRes = VetVisit rabid <$> vvVetRes <*> date <*> vvProblemRes <*> vvProceduresRes <*> vvNotesRes <*> vvSpayRes <*> vvCostRes 
    let vwidget = do 
         toWidget
             [lucius|
              #fvVisit div {
                 float:float;
                 margin:1%;
                }
              #fvInline input {
                   display:inline;
                }
               #fvBlock input {
                   display:block;
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
              #fvSpay {
                   width:40%}
              #fvCost { width:40%}
              #fvProblem, #fvProcedures, vfNotes {width:98%}
                 ##{fvId vvDateView} {display:inline}
                 ##{fvId vvVetView} {display:inline; width:15em;}
                 ##{fvId vvCostView} {display:inline; width:15 em;}    
                 ##{fvId vvSpayView} {display:inline; }
                 ##{fvId vvProblemView} {
                     display:block;
                     width:30em;
                 }
                 ##{fvId vvProceduresView} { width:30em; display:block; }
                 ##{fvId vvNotesView} {width:30em; display:block;}
                 
                 |]
         [whamlet|
            #{extra}
           <div #fvVisit>
            <div #fvInline>
             <div #fvDate>
               Date:  ^{fvInput vvDateView}
             <div #fvVet>
               Vet:   ^{fvInput vvVetView}
             <div #fvSpay>
              Alter/Euthen:   ^{fvInput vvSpayView}
             <div #fvCost>
               Cost:  ^{fvInput vvCostView}
            <div #fvBlock>
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
              <form method=post action=@{VetPostR rabid} enctype=#{enctype}>
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

