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
import BaseForm

getAges yrs diffMnths= do
    b1 <-queryStatus "BunnyLuv"
    today <- liftIO getCurrentDay
    let bday = addDays (yrs*(-365)) today
    let mnthDays = 31*diffMnths
    let result = sortEnt mnthDays bday b1
    let ageTit= "Rabbits within " ++ (show diffMnths) ++ " months of age "++(show yrs)
    base (toHtml ageTit) result



postGetAgeR::Handler Html
postGetAgeR  = do
  ((result, _), _) <- runFormPost getAgeForm
  case result of
       FormSuccess (AgeSearch age ageDiffMnths) ->getAges age ageDiffMnths
       _ -> redirect HomeR
  
queryAltered value = runDB $ do
  zipt<-select $ from $ \r->do
    if value=="No" then
     where_ ((r ^. RabbitAltered ==. val "No") ||. (r^. RabbitAltered ==. val "Unknown"))
     else
      where_ ((r ^. RabbitAltered ==. val "Spayed") ||. (r^. RabbitAltered ==. val "Neutered"))
    orderBy [asc (r ^. RabbitAltered), asc (r ^. RabbitName)]
    return r
  return zipt

  
query field value= runDB $ do
  zipt<-select $ from $ \r->do
    where_ (r ^. field ==. val value)
    orderBy [asc (r ^. RabbitName)]
    return r
  return zipt


querySource source = runDB $ do
  zipt<-select $ from $ \r ->do
     where_ (r ^. RabbitSourceType ==. val source)
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
getTestR = 
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
    FormSuccess name -> redirect (ShowNameR name)
    _ -> redirect HomeR



getHomeR :: Handler Html
getHomeR = do
    zinc <-queryStatus "BunnyLuv"
    base "BunnyLuv Rabbits" zinc

ageDiff::Day->Entity Rabbit->(Integer, Entity Rabbit)
ageDiff bday rabE@(Entity _ rab) = ( abs (diffDays bday (rabbitBirthday rab)), rabE)

clean::Integer->[(Integer, Entity Rabbit)]->[(Integer, Entity Rabbit)]
clean ageDiff = filter (\(a,_)-> a <= ageDiff) 

sortImp::(Integer, Entity Rabbit)->(Integer, Entity Rabbit)->Ordering
sortImp (a, r1) (b, r2)
          | a>b = GT
          | a==b = EQ
          | a<b = LT

extractRabb (a, rabE) = rabE
                  
sortEnt::Integer->Day->[Entity Rabbit]->[Entity Rabbit]
sortEnt ageRange bday rabs = nrabs where
        ageDiffs = map (ageDiff bday) rabs
        cleaned = clean ageRange ageDiffs
        sorted =  (sortBy sortImp cleaned)
        nrabs = map extractRabb sorted
  
getAgesR::Integer->Handler Html
getAgesR yrs = do
    b1 <-queryStatus "BunnyLuv"
    today <- liftIO getCurrentDay
    let bday = addDays (yrs*(-365)) today
    let result = sortEnt ageDiffMax bday b1
    let ageTit= "Rabbits within 2 years of age "++(show yrs)
    base (toHtml ageTit) result
 
