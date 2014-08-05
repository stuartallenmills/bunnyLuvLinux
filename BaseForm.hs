{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module BaseForm where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (||.))
import Yesod.Default.Util
import Yesod.Auth
import Foundation

import Data.Text (Text, unpack, append)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Data.Time
import Data.List (sortBy)
import qualified Data.Text as T
import Text.Julius
import Utils
import Control.Applicative


ageDiffMax::Integer
ageDiffMax = 2 * 365  -- 2 years in days

data AgeSearch = AgeSearch { agesearchAge::Integer
                             ,  agesearchDiff::Integer
                                }

getAgeForm::Html->MForm Handler (FormResult AgeSearch, Widget)
getAgeForm extras= do
  let fs = FieldSettings "sNamel" (Just "Find rabbit") (Just "getAge") (Just "stName") []
  (ageRes,ageView) <- mreq intField fs  Nothing
  (ageDiffRes, ageDiffView) <-mreq intField "bbb" (Just 12)
  let agesch = AgeSearch <$> ageRes <*> ageDiffRes
  let awid = do
        $(widgetFileNoReload def "cancelButton")
        [whamlet| #{extras}
             <div #getAgeDiv>
              <div #ageTitle style="margin-bottom:8px;">
                Find rabbits by age:
              <div #ageInD>
               <label for="getAge">Age: </label> ^{fvInput ageView} yrs
              <div #ageDiffD>
               <label for="ageDiff">Plus/Minus: </label> ^{fvInput ageDiffView} mnths
              <div .cancelBut #ageCan style="text-align:left; float:right;">
                                    <a href=@{HomeR}> cancel</a>
              <input #agesub type=submit value="find" sytle="float:none; margin-top:10px;">
         |]
        toWidget [lucius|
                    ##{fvId ageView} {
                             width:4em;
                      }
                    ##{fvId ageDiffView} {
                         width:4em;
                      }
                    #getAgeDiv div {
                         margin-top:2px;
                         margin-bottom:2px;
                      }
                    #getAgeDiv label {
                             display:inline-block;
                             width:40%;
                      }
                    #getAgeDiv .cancelBut {
                           height:1.5em;
                           width:3em;
                           padding-left:1px;
                     }
                    #agesub {
                       height: 1.7em;
                      }
                  |]
  return(agesch, awid)

getAgeWidget wid enctype = do
      [whamlet|
        <form #ageForm method=post action=@{GetAgeR} enctype=#{enctype}>
            ^{wid}
          |]
      toWidget [lucius|
           #ageForm {
                 background:#efefef;           
                 padding:10px;
                 border:thin solid #7f7f7f;
                 margin:0;
                 position:absolute;
                 width:25%;
                 transform: translate(80%, -45px);
                 box-shadow: 2px 2px 3px #2f2f2f;
                 z-index:100;
                 display:none;
               }
           #ageForm:after {
                 display:none;
            }
           #ageForm input {
               display:inline;
             }
          |]
      toWidget [julius|
                $( "#blAge" ).click(function() {
                   $( "#ageForm" ).show();
                  });
           |]
                 
 
doRabbitRow::Day->RabbitId->Rabbit->Widget
doRabbitRow today rabbitid rabbit = $(widgetFileNoReload def "rabRow") 

base atitle result  = do 
     (formWidget, enctype) <- generateFormPost getNameForm
     (ageWidget, age_enctype) <-generateFormPost getAgeForm
     bnames <- liftIO getNamesDB
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     let isAuth=(auth==Authorized)
     today<- liftIO $ getCurrentDay
     let numBuns = length result
     
     let numBunsStr = "  : (found " ++ (show numBuns) ++ " )"
     
     defaultLayout $ do
        setTitle atitle
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
        addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
        addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"
        toWidget [julius| $( document ).ready(function(){
                             if (#{isAuth}) { 
                              $( "#blReports" ).show(); 
                              $( "#blStatus" ).show();
                              $( "#blSource" ).show();      
                              $( "#rAddM" ).show();   
                              $( "#blAdmin" ).show(); }
                             else {
                              $( "#blReports" ).hide(); 
                              $( "#blStatus" ).hide();
                              $( "#blSource" ).hide();      
                              $( "#rAddM" ).hide(); 
                              $( "#blAdmin" ).hide(); }
                           });

                  $(function() {
                     $( document ).tooltip();
                     });


                             |]
        toWidget [lucius| #atitleD {
                                width:100%;
                                float:left;
                                text-align:center;
                                background:#e8e8e8;
                                padding-bottom:5px;
                                padding-top:5px;
                                border-bottom:thin solid #404040;
                            }

                           .ui-tooltip-content {
                              font-size:0.8em;
                           }
                      
              |]

        [whamlet|
         ^{getNameWidget bnames formWidget enctype}
         ^{headerLogWid imgpath maid}
         ^{mainMenu}
         ^{getAgeWidget ageWidget age_enctype}
        <div #formResults>
         <div #atitleD> 
              <b> #{atitle} #{numBunsStr}

         $forall Entity rabbitid rabbit <- result
           ^{doRabbitRow today rabbitid rabbit }
                |]

