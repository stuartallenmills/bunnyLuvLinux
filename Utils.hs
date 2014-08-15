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
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Data.Time
import Data.List (sortBy)
import qualified Data.Text as T
import Text.Julius


queryGetBonded rabId = runDB $ 
  select $
  from $ \(rab, bonded) -> do
  where_ ((bonded ^.BondedFirst ==. val rabId) &&. (rab ^. RabbitId ==. bonded ^. BondedSecond))
  return (rab, bonded)

  
queryStatus status = runDB $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. RabbitStatus ==. val status)
     orderBy [asc (r ^. RabbitName)]
     return (r)
  return zipt


queryName name = runDB $ do
  let (f,s) = T.splitAt 1 name
  let capName = append (T.toUpper f) s
  let lowName = append (T.toLower f) s
      
  zipt<-select $ from $ \r ->do
     where_ ((like  (r ^. RabbitName)  ((%) ++. val capName ++. (%)) ) ||.
             (like  (r ^. RabbitName)  ((%) ++. val lowName ++. (%)) ) 
             )
     orderBy [asc (r ^. RabbitName)]
     return r
  return zipt

rabId2rab rabId = do
         Just rab <- runDB $ get rabId
         return rab
         
gostring::[T.Text]->T.Text
gostring alist = out where
           temp= foldl (\accum x-> T.append( T.append (T.append "\"" x) "\",") accum) T.empty alist
           out = T.append "["  (T.append (T.take ((T.length temp) - 1) temp) "]")

getName::Entity Rabbit->Text
getName (Entity rabId rab) = rabbitName rab

getNames::[Entity Rabbit]->[Text]
getNames  = map getName 

getNamesDB:: Handler [Text]
getNamesDB = do
     rabs<-queryStatus "BunnyLuv"
     return (getNames rabs)

getNameForm::Html->MForm Handler (FormResult Text, Widget)
getNameForm extra = do
  let fs = FieldSettings "sNamel" (Just "Find rabbit by name") (Just "getName") (Just "nameField") []
  (nameRes, nameView) <- mreq textField fs  Nothing
  let wid =do
                  [whamlet| #{extra}
                     <div class=ui-widget #getNameDiv style="font-size:1em; display:inline" title="Find rabbit by name">
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
                             .ui-autocomplete {
                                   z-index:100;
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
                    $( "#getName" ).attr("title", "Find rabbit by name");
                    $( "#getName" ).autocomplete({
                      source: #{rawJS (gostring bnames)},
                      select: function (event, ui) {
                          $( "#getName" ).val (ui.item.label);
                          $( "#formName" ).submit()
                        }
                    });
                   });
               |]
