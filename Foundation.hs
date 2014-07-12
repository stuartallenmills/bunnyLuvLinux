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
import Data.Text (Text, pack)
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
    firstName Text
    lastName Text
    phone Text
    street Text
    city   Text
    state  Text
    zip    Text
    PhoneKey phone
    deriving Show

Died 
    date Text
    cause Text
    deriving Show
    
Adopted
    rabbit RabbitId
    date Day
    person Person
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
   
VetVisit
   rabbit RabbitId
   vet Text
   date Day
   problem Text
   procedures Text
   notes Text
   spay Text
   cost Double Maybe
   deriving Show

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
   dateIn Day
   sourceType Text
   source Text
   sex Text
   altered Text
   alteredDate Day Maybe
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
status = [ ("BunnyLuv", "BunnyLuv"), ("Adopted", "Adopted"), ("Died", "Died"), ("Euthanized", "Euthanized")]
vets::[(Text, Text)]
vets = [("Dr. Misetich", "Dr. Misetich"), ("Dr. Petritz", "Dr. Petritz"), ("Dr. Steele (C.A.R.E)", "Dr. Steele (C.A.R.E)")]
procedures::[(Text,Text)]
procedures=[("Spayed", "Spayed"), ("Neutered", "Neutered"), ("Euthanized", "Euthanized"), ("Other", "Other")]


                                      

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

showtime::Day->Text
showtime time = pack (formatTime defaultTimeLocale "%m/%d/%Y" time)

text2date::FormResult Text -> FormResult Day
text2date tdate =  fmap (doparseTime.unpack) tdate

text2dateM::FormResult (Maybe Text)->FormResult (Maybe Day)
text2dateM tdateM= fmap (fmap (doparseTime.unpack)) tdateM

getLocalTime:: IO (LocalTime)
getLocalTime = do 
  ct<-getCurrentTime
  tz<-getCurrentTimeZone
  let local_time = utcToLocalTime tz ct
  return (local_time)

-- END  TIME ROUTINE

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

sn = Person "Stuart" "Mills" "818-884-5537" "23425 Kilty" "West Hills" "CA" "91307"
lulu = Rabbit "Lulu" "white terrorist" (doparseTime "11/10/2009") "Shelter" "East valley shelter"
       "F" "No" Nothing  "approx 6 months" "Adopted" "11/11/2011" "Sharon Mills"

openConnectionCount :: Int
openConnectionCount = 10



initDB::IO ()
initDB = do
  ct<-getCurrentTime
  tz<-getCurrentTimeZone
  let local_time = utcToLocalTime tz ct
  withSqlitePool "test5.db3" openConnectionCount $ \pool -> do
   runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" "818-970-6052" "101 welby Way" "paris" "france" "12314"
        stu<-insert sn
        luid<-insert $ lulu 
        chid<-insert $ Rabbit "Chester" "black bunny nice" (doparseTime "08/09/2001")  "Shelter" "West Valley Shelter" "M"  "Neutered" (Just (doparseTime "2/2/2003")) "approx 1 yr" "Died" "07/08/2005" "Unknown Causes"
        jid<-insert $ Rabbit "Joan" "weird ass lop" (doparseTime "07/02/2013") "Other"  "Rescued by Britta Menges-lady to take to kill shelter" "F" "Unknown" Nothing "approx 2 yrs" "BunnyLuv" "07/07/1999" "in group 3"
        insert $ Wellness jid ((doparseTime  "08/07/2013")) False Nothing (Weight 3 4) "Bad JuJu" "vooDoo" "Stuart"
        insert $ Wellness jid ((doparseTime  "11/10/2013")) True (Just 102.5) (Weight 3 6) "Healthy" "none" "Doug"
        insert $ Wellness jid  ( (doparseTime  "12/11/2013")) False Nothing (Weight 3 2) "Stubbed toe" "Vet for stubbed toe" "Sharon"
        insert $ Wellness jid ((doparseTime  "1/12/2014"))  False (Just 101.2) (Weight 3 1) "none" "none" "Paul"
        ard<-insert $ Vet "Arden the Vet" "MisaFit" "818-970-6051"
        jona<-insert $ Vet "We Like Pets" "Jabba" "310-642-5947"
        insert $ VetVisit luid "Arden the Vet" (doparseTime "2/12/2014") "Needs to be altered" "Spayed" "Went Well" "Spade" (Just 295.45)
        insert $ VetVisit chid "We Like Pet" (doparseTime "3/22/2014") "Acting Sick" "Test indicate Pnenomia" "baytril prres" "Other" Nothing
        insert $ Adopted luid (doparseTime "2/14/2014") sn
        return ()
