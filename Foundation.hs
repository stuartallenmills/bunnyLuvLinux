{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, GADTs, FlexibleContexts #-}

module Foundation where

import Conduit
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Hamlet
import Yesod hiding (parseTime)
import Yesod.Default.Util
import Data.Text (Text, unpack)
import Database.Esqueleto
import Database.Persist.Sqlite --(runSqlite, runMigrationSilent, withSqlitePool)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Time
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Locale

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    phone String
    PhoneKey phone
    deriving Show

Died 
    date String
    cause String
    deriving Show
    
Adopted
    date String
    rabbit RabbitId
    person PersonId
    deriving Show

Weight 
    lbs Int
    oz  Int
    deriving Show

Vet
   practice Text
   doctor   Text
   telephone Text
   deriving Show
   
VetVist
   rabbit RabbitId
   vet VetId
   date Day
   procedures Text
   spay Bool
   
Wellness
    rabbit RabbitId
    date Day
    groomed Bool
    temp  Double Maybe
    weight Weight
    notes Text
    treatment Text
    responsible Text
    deriving Show
    
Rabbit
   name Text
   desc Text
   dateIn Text
   sourceType Text
   source Text
   sex Text
   altered Text
   ageIntake Text
   status Text
   statusDate Text
   statusNote Text
   deriving Show
|]

sourceType::[(Text,Text)]
sourceType=[("Shelter","Shelter"), ("Other", "Other")]

sex::[(Text,Text)]
sex =[("F","F"), ("M","M")]
altered::[(Text,Text)]
altered = [("Spayed", "Spayed"), ("Neutered", "Neutered"), ("No", "No"), ("Unknown", "Unknown")]
status::[(Text, Text)]
status = [ ("BunnyLuv", "BunnyLuv"), ("Adopted", "Adopted"), ("Died", "Died"), ("Euthenized", "Euthenized")]


data App = App (TVar [(Text, ByteString)])

-- instance Yesod App
instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

mkYesodData "App" $(parseRoutesFile "config/routes")

--  TIME ROUTINES 
doparseTime::String->Day
doparseTime  st = readTime defaultTimeLocale "%-m/%-d/%-Y" st

showtime time = formatTime defaultTimeLocale "%m/%d/%Y" time

getList :: Handler [Text]
getList = do
    App tstate <- getYesod
    state <- liftIO $ readTVarIO tstate
    return $ map fst state

addFile :: App -> (Text, ByteString) -> Handler ()
addFile (App tstore) op =
    liftIO . atomically $ do
        modifyTVar tstore $ \ ops -> op : ops

getById :: Text -> Handler ByteString
getById ident = do
    App tstore <- getYesod
    operations <- liftIO $ readTVarIO tstore
    case lookup ident operations of
      Nothing -> notFound
      Just bytes -> return bytes

sn = Person "Stuart" "Mills" "818-884-5537"
lulu = Rabbit "Lulu" "white terrorist" "11/10/2009" "Shelter" "East valley shelter"
       "F" "Spayed" "approx 6 months" "Adopted" "11/11/2011" "Sharon Mills"

openConnectionCount :: Int
openConnectionCount = 10

initDB::IO ()
initDB = do
  ct<-getCurrentTime
  tz<-getCurrentTimeZone
  withSqlitePool "test5.db3" openConnectionCount $ \pool -> do
   runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" "818-970-6052"
        stu<-insert sn
        luid<-insert $ lulu 
        chid<-insert $ Rabbit "Chester" "black bunny nice" "08/09/2001"  "Shelter" "West Valley Shelter" "M"  "Neutered" "approx 1 yr" "Died" "07/08/2005" "Unknown Causes"
        jid<-insert $ Rabbit "Joan" "weird ass lop" "07/02/2013" "Other"  "Rescued by Britta Menges-lady to take to kill shelter" "F" "Unknown" "approx 2 yrs" "BunnyLuv" "07/07/1999" "in group 3"
        insert $ Wellness jid ((doparseTime  "08/07/2013")) False Nothing (Weight 3 4) "Bad JuJu" "vooDoo" "Stuart"
        insert $ Wellness jid ((doparseTime  "11/10/2013")) True (Just 102.5) (Weight 3 6) "Healthy" "none" "Doug"
        insert $ Wellness jid  ( (doparseTime  "12/11/2013")) False Nothing (Weight 3 2) "Stubbed toe" "Vet for stubbed toe" "Sharon"
        insert $ Wellness jid ((doparseTime  "1/12/2014"))  False (Just 101.2) (Weight 3 1) "none" "none" "Paul"
        return ()
