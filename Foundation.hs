{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, GADTs, FlexibleContexts #-}

module Foundation where

import Conduit
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Text.Hamlet
import Yesod hiding (parseTime)
import Yesod.Default.Util
import Yesod.Auth
import Yesod.Auth.Message
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
import qualified Data.Map.Strict as Map
import Filesystem


-- CONFIGURATION
imagesURL = "http://192.168.1.128:3000/images/"

uploadDirectory :: FilePath
uploadDirectory = "C:/shared/msys64/home/smills/Hask/newscott/Images"


getImagePath = readTextFile "imagepath"
getUploadDir = readTextFile "uploaddir"

getImage::String->String->String
getImage a b = a++b

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

Usr
    loginName Text
    password Text
    UniqueLoginName loginName
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

Died
  date Text
  notes Text
  deriving Show

Wellness
    rabbit RabbitId
    date Day
    groomed Bool
    temp  Double Maybe
    weight Weight
    notes Text
    treatment Textarea
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
   status Text
   statusDate Text
   statusNote Text
   birthday Day
   image Text Maybe
   deriving Show
|]


type UName = Text
type UPass = Text
type UsrMap = Map.Map Text Text





showgroomed::Wellness->Text
showgroomed wellR = if (wellnessGroomed wellR) then "Y" else "-"

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


               

data App = App
  { httpManager :: Manager
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

-- instance Yesod App
instance Yesod App where
  authRoute _ = Just $ AuthR LoginR
  isAuthorized PostR True = isAdmin
  isAuthorized AddR _ = isAdmin
  isAuthorized (EditR _) _ = isAdmin
  isAuthorized (WellnessR _) _ = isAdmin
  isAuthorized (DiedR _) _ = isAdmin
  isAuthorized (VetVisitR _) _ = isAdmin
  isAuthorized (AdoptedR _) _ = isAdmin
  isAuthorized (UpdateR _) True = isAdmin
  isAuthorized (ImagesR _) _ = isAdmin
  
  isAuthorized _ _ = return Authorized
  
  defaultLayout widget = do
--    mmsg <-getMessage
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage




-- AUTHORIZATION
instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [authBunnyluv]

    authHttpManager = httpManager

    maybeAuthId = lookupSession "_ID"

-- authenication
isAdmin::HandlerT App IO AuthResult
isAdmin = do
    usrsMap <- liftIO $ getUsrs
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> Unauthorized "You must log in to an authorized account" --AuthenticationRequired
        Just usr -> if (Map.member usr usrsMap) then Authorized
          else  Unauthorized "Your account is  not authorized"

--  HTML  CSS ELEMENTS
cssmenuWidget::Widget
cssmenuWidget = $(widgetFileNoReload def "cssmenu")

mainMenu::Widget
mainMenu = do
          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
          cssmenuWidget

-- header
        
headerLogWid imgpath maid = $(widgetFileNoReload def "headerLog")
headerwidget = $(widgetFileNoReload def "header")


--  TIME ROUTINES 
doparseTime::String->Day
doparseTime  st = readTime defaultTimeLocale "%-m/%-d/%-Y" st

showtime::Day->Text
showtime time = pack (formatTime defaultTimeLocale "%m/%d/%Y" time)

showfiletime time = (formatTime defaultTimeLocale "%m_%d_%Y" time)

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

getCurrentDay = do
    local_time <- getLocalTime
    let today = localDay local_time
    return (today)

getYrsDateInM::Maybe Rabbit-> Maybe Integer
getYrsDateInM  Nothing = Nothing
getYrsDateInM  (Just rab) = Just (getCurrentYears (rabbitDateIn rab) rab)

getCurrentYears::Day->Rabbit-> Integer
getCurrentYears today rab = years where
     dage = diffDays today  (rabbitBirthday rab)
     years = dage `div` 365

getMonthsDateInM::Maybe Rabbit->Maybe Integer
getMonthsDateInM Nothing = Nothing
getMonthsDateInM  (Just rab) = Just (getCurrentMonths (rabbitDateIn rab) rab)

getCurrentMonths::Day->Rabbit-> Integer
getCurrentMonths today rab = mnths where
     dage = diffDays today  (rabbitBirthday rab)
     (_,rm) = dage `divMod` 365
     tmnths = rm `div` 30
     mnths = if tmnths > 11 then 11 else tmnths


-- END  TIME ROUTINE

-- Authorization


authBunnyluv :: YesodAuth m => AuthPlugin m
authBunnyluv =
    AuthPlugin "bunnyluv" dispatch login
  where
    dispatch "POST" [] = do
        ident <- lift $ runInputPost $ ireq textField "ident"
        usrsMap <- liftIO  getUsrs
        let isOnFile = Map.member ident usrsMap
        let upass = if isOnFile then usrsMap Map.! ident else "guest"
        pass <-lift $ runInputPost $ ireq passwordField "pass"
        if (pass == upass)   then
         lift $ setCredsRedirect $ Creds "bunnyluv" ident []
        else loginErrorMessageI  LoginR PassMismatch

    dispatch _ _ = notFound
    url = PluginR "bunnyluv" []
    login authToMaster = do
        msg <- getMessage
        headerwidget
        toWidget 
           [whamlet|
$newline never

<div #logTitle>
    <b> BunnyLuv Login

<form method="post" action="@{authToMaster url}">
    <p>Username: #
     <input type="text" name="ident">

    <p>Password: #
     <input type="password" name="pass">
    <p>
    <input type="submit" value="Login">
    $maybe tmsg <-msg
       <p> #{ tmsg}

|]

usrInsert umap (Entity uId (Usr nme pss))  = Map.insert nme pss umap

getUsrsDb = runSqlite "test5.db3" $ do
    tusrs <- select $ from $ \person-> do
      return person
    return tusrs

getUsrs  :: IO (UsrMap)
getUsrs  = do
    tusrs <- getUsrsDb
    return $  foldl (\accum eusr->usrInsert accum eusr) Map.empty tusrs
    
                                     

-- Database init

sn = Person "Stuart" "Mills" "818-884-5537" "23425 Kilty" "West Hills" "CA" "91307"
lulu = Rabbit "Lulu" "white terrorist" (doparseTime "11/10/2009") "Shelter" "East valley shelter"
       "F" "No" Nothing   "Adopted" "11/11/2011" "Sharon Mills" (doparseTime "1/1/2007") (Just "Test1.jpg")

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
        insert $ Usr "sharon" "bunnyluv"
        insert $ Usr "stuart" "jrr1jrr1"
        
        insert $ Person "Michael" "Snoyman" "818-970-6052" "101 welby Way" "paris" "france" "12314"
        stu<-insert sn
        luid<-insert $ lulu 
        chid<-insert $ Rabbit "Chester" "black bunny nice" (doparseTime "08/09/2001")  "Shelter" "West Valley Shelter" "M"  "Neutered" (Just (doparseTime "2/2/2003")) "Died" "07/08/2005" "Unknown Causes" (doparseTime "1/1/1998") Nothing
        jid<-insert $ Rabbit "Joan" "weird ass lop" (doparseTime "07/02/2013") "Other"  "Rescued by Britta Menges-lady to take to kill shelter" "F" "Unknown" Nothing "BunnyLuv" "07/07/1999" "in group 3" (doparseTime "1/1/1998") Nothing
        insert $ Wellness jid ((doparseTime  "08/07/2013")) False Nothing (Weight 3 4) "Bad JuJu" (Textarea "vooDoo") "Stuart"
        insert $ Wellness jid ((doparseTime  "11/10/2013")) True (Just 102.5) (Weight 3 6) "Healthy" (Textarea "none") "Doug"
        insert $ Wellness jid  ( (doparseTime  "12/11/2013")) False Nothing (Weight 3 2) "Stubbed toe" (Textarea "Vet for stubbed toe") "Sharon"
        insert $ Wellness jid ((doparseTime  "1/12/2014"))  False (Just 101.2) (Weight 3 1) "none" (Textarea "none") "Paul"
        ard<-insert $ Vet "Arden the Vet" "MisaFit" "818-970-6051"
        jona<-insert $ Vet "We Like Pets" "Jabba" "310-642-5947"
        insert $ VetVisit luid "Arden the Vet" (doparseTime "2/12/2014") "Needs to be altered" "Spayed" "Went Well" "Spade" (Just 295.45)
        insert $ VetVisit chid "We Like Pet" (doparseTime "3/22/2014") "Acting Sick" "Test indicate Pnenomia" "baytril prres" "Other" Nothing
        insert $ Adopted luid (doparseTime "2/14/2014") sn
        return ()
