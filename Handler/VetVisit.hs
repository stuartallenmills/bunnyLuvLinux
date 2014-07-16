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
import Yesod hiding ((!=.), (==.), (=.), update)
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
import Data.Time.LocalTime


testAltered::Rabbit->[(Text,Text)]
testAltered rab | (((rabbitAltered rab)=="Spayed") || ((rabbitAltered rab)=="Neutered")) = []
                | ((rabbitSex rab)=="F") = [("Spayed", "Spayed")]
                | otherwise = [("Neutered", "Neutered")]

setSources::Rabbit->[(Text,Text)]
setSources rab = [("Other","Other")]++(testAltered rab) ++ [("Euthanized","Euthanized")]

headerWidget::Widget
headerWidget = $(widgetFileNoReload def "header")

vetVisitForm::Rabbit-> RabbitId->Html-> MForm Handler (FormResult VetVisit, Widget)
vetVisitForm rab rabid extra = do
    local_time <- liftIO $ getLocalTime
    let stime = showtime (localDay local_time)
    (vvDateRes, vvDateView)<-mreq textField "nope" (Just stime)
    (vvVetRes, vvVetView)<-mreq (selectFieldList vets) "nope" Nothing
    (vvProblemRes, vvProblemView)<-mreq textField "nopte" Nothing
    (vvProceduresRes, vvProceduresView)<-mreq textField "n" Nothing
    (vvNotesRes, vvNotesView)<-mreq textField "n" Nothing
    (vvSpayRes, vvSpayView)<-mreq (selectFieldList (setSources rab)) "n" Nothing
    (vvCostRes, vvCostView)<-mopt doubleField "n" Nothing
    let date = text2date vvDateRes
    let vetvisitRes = VetVisit rabid <$> vvVetRes <*> date <*> vvProblemRes <*> vvProceduresRes <*> vvNotesRes <*> vvSpayRes <*> vvCostRes 
    let vwidget = do
         addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
         toWidget
             [julius|
              var h7 =document.getElementById("hident7");
              var h5 =document.getElementById("hident5");
              var h4 =document.getElementById("hident4");
              var h6 =document.getElementById("hident6");
              h7.onchange=function(){
              var selectedText = h7.options[h7.selectedIndex].text;
              if (selectedText != "Other") {
                    h5.value =selectedText;
                    if (selectedText !="Euthanized") {
                         h4.value = "Not altered";
                         h6.value = "None";
                    }
                    else {
                        h4.value ="";
                        h6.value="";
                    }
              }
              else{ h4.value=""; h6.value=""; h5.value=""; }
               
              }
            |]
         toWidget
             [lucius|
              .vvRw {
                width:100%;
                border-bottom:1px dashed #999999;
              }
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
               Altered/Euthanized:   ^{fvInput vvSpayView}
              <div #fvCost>
                Cost:  ^{fvInput vvCostView}
            <div #fvBlock>
               <div #vfProblem>
                   Problem:   ^{fvInput vvProblemView}
              <div #vfProcedures>
                  Procedures:   ^{fvInput vvProceduresView}
              <div #fvNotes>
                  Notes:  ^{fvInput vvNotesView}
             <input type=submit value="submit">
            |]
    return (vetvisitRes, vwidget)
        
getVetVisitR ::RabbitId->Handler Html
getVetVisitR rabid = do
    Just rab <-runSqlite "test5.db3"  $ do
                  rabt<- get rabid
                  return rabt
    (formWidget, enctype) <- generateFormPost (vetVisitForm rab rabid)
    defaultLayout $ do
         setTitle "Vet Visit" 
         $(widgetFileNoReload def "cancelbutton")
         [whamlet|
             ^{headerWidget}
              <div #vvTitle .subTitle>
                <b> Vet Visit for &nbsp; #{rabbitName rab}
                <div #vvCan style="float:right; display:inline;">
                  <div .cancelBut #vvEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabid}> cancel </a>

              <form method=post action=@{VetPostR   rabid} enctype=#{enctype}>
                 ^{formWidget}
          |]
  

postVetPostR::RabbitId->Handler Html
postVetPostR  rabID = do
  Just rab <-runSqlite "test5.db3"  $ do
                  rabt<- get rabID
                  return rabt
  (((result), _), _) <-runFormPost (vetVisitForm rab rabID)

  case result of
    FormSuccess vetVisit -> do
      runSqlite "test5.db3" $ do
        _ <-insert  vetVisit
        return ()
      if (((vetVisitSpay vetVisit) == "Neutered") || ((vetVisitSpay vetVisit) == "Spayed"))
       then
        runSqlite "test5.db3" $
         do  update $ \p -> do 
              set p [ RabbitAltered =. val (vetVisitSpay vetVisit), RabbitAlteredDate =. val (Just (vetVisitDate vetVisit))]
              where_ (p ^. RabbitId ==. val rabID)
              return ()
          else
            return ();
      if ((vetVisitSpay vetVisit) == "Euthanized")
       then
        runSqlite "test5.db3" $
         do  update $ \p -> do 
              set p [ RabbitStatus =. val (vetVisitSpay vetVisit), RabbitStatusDate =. val (showtime (vetVisitDate vetVisit))]
              where_ (p ^. RabbitId ==. val rabID)
              return ()
        else
            return ();
            
    _ -> return ()

  redirect (ViewR rabID)

