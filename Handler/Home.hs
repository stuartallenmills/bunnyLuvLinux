{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Home where

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

ageDiffMax::Integer
ageDiffMax = 2



queryAltered value =runSqlite bunnyLuvDB $ do
  zipt<-select $ from $ \r->do
    if value=="No" then
     where_ ((r ^. RabbitAltered ==. val "No") ||. (r^. RabbitAltered ==. val "Unknown"))
     else
      where_ ((r ^. RabbitAltered ==. val "Spayed") ||. (r^. RabbitAltered ==. val "Neutered"))
    orderBy [asc (r ^. RabbitAltered), asc (r ^. RabbitName)]
    return r
  return zipt

  
query field value= runSqlite bunnyLuvDB $ do
  zipt<-select $ from $ \r->do
    where_ (r ^. field ==. val value)
    orderBy [asc (r ^. RabbitName)]
    return r
  return zipt


querySource source = runSqlite bunnyLuvDB $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. RabbitSourceType ==. val source)
     orderBy [asc (r ^. RabbitName)]
     return (r)
  return zipt

queryName name = runSqlite bunnyLuvDB $ do
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


getAlteredR isAlt = do
     zinc<- queryAltered isAlt
     let ti = if (isAlt=="No") then "Not Altered" else "Altered"
     base ti  zinc

getAllR = do
    bl <-queryStatus "BunnyLuv"
    ad <-queryStatus "Adopted"
    di <-queryStatus "Died"
    eu <-queryStatus "Euthanized"
    let zinc = bl++ad++di++eu
    base "All Rabbits" zinc
  
getQueryR status  = do
     zinc<- queryStatus status
     let ti = append "Status: " status
     base (toHtml ti) zinc
     
getSourceR source  = do
    zinc<- querySource source
    let ti = append "Source: " source
    base (toHtml ti)  zinc

doRabbitRow::Day->RabbitId->Rabbit->Widget
doRabbitRow today rabbitid rabbit = $(widgetFileNoReload def "rabRow") 

goEdit (Entity rabid rabbit) =
  redirect (ViewR rabid)
  
getShowNameR::Text->Handler Html
getShowNameR name = do
  zinc<-queryName name
  page<- if (length zinc)== 1 then
           goEdit (Prelude.head zinc)
         else do        
    let ti = append "Name: " name
    base (toHtml ti) zinc
  return page
   
getTestR :: Handler Html
getTestR = do
  defaultLayout $ do
     setTitle "Test"
     [whamlet|
      ^{mainMenu}
      <div>This is a test of the something
            |]

postNameR::Handler Html 
postNameR = do
  ((result,_), _) <- runFormPost getNameForm
  case result of
    FormSuccess name -> (redirect (ShowNameR name))
    _ -> redirect (HomeR)




base atitle result = do 
     (formWidget, enctype) <- generateFormPost getNameForm
     bnames <- liftIO getNamesDB
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     let isAuth=(auth==Authorized)
     today<- liftIO $ getCurrentDay
     let numBuns = length result
     
     let numBunsStr = " : ( " ++ (show numBuns) ++ " )"
     
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
         <div #atitleD> 
              <b> #{atitle} #{numBunsStr}

         <div #rabbitContainer>
     $forall Entity rabbitid rabbit <- result
           ^{doRabbitRow today rabbitid rabbit }
                |]


getHomeR :: Handler Html
getHomeR = do
    zinc <-queryStatus "BunnyLuv"
    base "BunnyLuv Rabbits" zinc

ageDiff::Day->Entity Rabbit->(Integer, Entity Rabbit)
ageDiff bday rabE@(Entity _ rab) = ( abs (diffDays bday (rabbitBirthday rab)), rabE)

clean::[(Integer, Entity Rabbit)]->[(Integer, Entity Rabbit)]
clean  = filter (\x->fst x <=ageDiffMax) 

sortImp::(Integer, Entity Rabbit)->(Integer, Entity Rabbit)->Ordering
sortImp (a, r1) (b, r2)
          | a>b = GT
          | a==b = EQ
          | a<b = LT

extractRabb (a, rabE) = rabE
                  
sortEnt::Day->[Entity Rabbit]->[Entity Rabbit]
sortEnt bday rabs = nrabs where
        ageDiffs = map (ageDiff bday) rabs
        cleaned = clean ageDiffs
        sorted =  (sortBy sortImp cleaned)
        nrabs = map extractRabb sorted
  
getAgesR::Integer->Handler Html
getAgesR yrs = do
    b1 <-queryStatus "BunnyLuv"
    today <- liftIO getCurrentDay
    let bday = addDays (yrs*(-365)) today
    let result = sortEnt bday b1
    let ageTit= "Age: "++(show yrs)
    base (toHtml ageTit) result
 
