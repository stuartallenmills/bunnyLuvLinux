{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Utils where

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

queryStatus status = runSqlite bunnyLuvDB $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. RabbitStatus ==. val status)
     orderBy [asc (r ^. RabbitName)]
     return (r)
  return zipt

gostring::[T.Text]->T.Text
gostring alist = out where
           temp= foldl (\accum x-> T.append( T.append (T.append "\"" x) "\",") accum) T.empty alist
           out = T.append "["  (T.append (T.take ((T.length temp) - 1) temp) "]")

getName::Entity Rabbit->Text
getName (Entity rabId rab) = rabbitName rab

getNames::[Entity Rabbit]->[Text]
getNames  = map getName 

getNamesDB:: IO [Text]
getNamesDB = do
     rabs<-queryStatus "BunnyLuv"
     return (getNames rabs)

getNameForm::Html->MForm Handler (FormResult Text, Widget)
getNameForm extra = do
  let fs = FieldSettings "sNamel" (Just "Find rabbit") (Just "getName") (Just "stName") []
  (nameRes, nameView) <- mreq textField fs  Nothing
  let wid =do
                  [whamlet| #{extra}
                     <div class=ui-widget #getNameDiv style="font-size:1em; display:inline">
                         <label style="font-size:0.8em;" for="getName" >name: </label>  ^{fvInput nameView}
                     <input #nameIn type=submit value="find" style="display:none;">
                    |]
                  toWidget [lucius|
                               #getName {
                                      font-size:0.9em;
                                  }
                               #getNameDiv {
                                  float:right;
                                }

                             .ui-menu-item {
                                      font-size:0.8em;
                                }
                      |]
                  
  return (nameRes, wid)

getNameWidget bnames wid enctype = do
         [whamlet|
           <form #formName method=post action=@{NameR} enctype=#{enctype}>
            ^{wid}
           |]
         toWidget [lucius|
                           #formName {
                              padding:0;
                              border:none;
                              margin:0;
                              float:right;
                             }
                           #formName input {
                              display:inline;
                           }
            |]
         toWidget [julius|
                  $( document ).ready(function() { 
                   alert("Executing autocomplete")
                   $( "#getName" ).autocomplete({
                      source: #{rawJS (gostring bnames)},
                      minLength: 1,
                      select: function (event, ui) {
                          $( "#getName" ).val (ui.item.label);
                          $( "#formName" ).submit()
                        }
                    });
                   });
               |]
