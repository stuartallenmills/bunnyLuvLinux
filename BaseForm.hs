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
import AgeForm
import Control.Applicative



 
doRabbitRow::Day->RabbitId->Rabbit->Widget
doRabbitRow today rabbitid rabbit = $(widgetFileNoReload def "rabRow")
doRabbitRows::Day->[Entity Rabbit]->Widget
doRabbitRows today result = $(widgetFileNoReload def "rabbitRows")

base atitle result  = do 
     (formWidget, enctype) <- generateFormPost getNameForm
     (ageWidget, age_enctype) <-generateFormPost getAgeForm
     bnames <-  getNamesDB
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     let mode =  (maid == Just "demo")
     let isAuth=(auth==Authorized)
     today<- liftIO getCurrentDay
     let numBuns = length result
     let numBunsStr = "  : ("++ (show numBuns) ++ ")"
     
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
    $( document ).tooltip({
      position: {
        my: "center bottom-20",
        at: "center top",
        using: function( position, feedback ) {
          $( this ).css( position );
          $( "<div>" )
            .addClass( "arrow" )
            .addClass( feedback.vertical )
            .addClass( feedback.horizontal )
            .appendTo( this );
        }
      }
    });
  });
 

                             |]
        toWidget [lucius| #atitleD {
                                width:100%;
                                float:left;
                                text-align:center;
                                background:#efefef;
                                padding-bottom:5px;
                                padding-top:5px;
                                border-bottom:thin solid #404040;
                                margin-bottom:5px;
                            }

                      
              |]

        [whamlet|
        <div #blHeaderD>
         ^{getNameWidget bnames formWidget enctype}
         ^{headerLogWid imgpath maid}
         ^{mainMenu mode}
         ^{getAgeWidget ageWidget age_enctype}
        <div #formResults>
         <div #atitleD> 
              <b> #{atitle} 
            <div #foundcount style="float:right; font-size:90%; padding-right:5px;"> #{numBunsStr}
         ^{doRabbitRows today result}
                |]

