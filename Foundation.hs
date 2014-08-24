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
import Yesod.Static
import Data.Text (Text, unpack)
import Database.Esqueleto
import Database.Persist.Sqlite --(runSqlite, runMigrationSilent, withSqlitePool)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
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



getImagePath = readTextFile "links/imagepath"
getUploadDir = readTextFile "links/uploaddir"
getBackup = readTextFile "links/backup"
getPort = readTextFile "links/port"

bunnyLuvDB::Text
bunnyLuvDB = "bunnyluv.db3"

demoDB::Text
demoDB = "demo.db3"

usrsDB::Text
usrsDB = "usrs.db3"

getImage::String->String->String
getImage a b = a++b

share [mkPersist sqlSettings, mkMigrate "migrateUsr"] [persistLowerCase|
Usr
    loginName Text
    password Text
    UniqueLoginName loginName
    deriving Show

|]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

DailyReport 
  date Day
  person Text
  report Textarea
  deriving Show

HomeCheck 
   date Day
   hcDay Day
   hcTime Text
   hcNotes Text Maybe
   hcResults Text Maybe
   deriving Show


Person
    firstName Text
    lastName Text
    phone Text
    mobile Text Maybe
    street Text
    apt Text Maybe
    city   Text
    state  Text
    zip    Text
    email  Text Maybe
    PhoneKey phone
    deriving Show

Treatment 
   rabbit RabbitId
   reason Text
   start Day
   stop  Day Maybe
   deriving Show

Instruction
   treatment TreatmentId
   desc Textarea
   frequency Text Maybe
   start Day
   stop  Day Maybe
   deriving Show

TreatmentB
   rabbit RabbitId
   start  Day
   reason Text
   instruct Textarea
   stop Day Maybe
   deriving Show

Bonded
  first RabbitId
  second RabbitId
  relation Text
  deriving Show

Adopted
    rabbit RabbitId
    date Day
    person Person
    notes Text Maybe
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

Relation
   rab1 Rabbit
   rab2 Rabbit
   kind::Text
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



type UsrMap = Map.Map Text Text





showgroomed::Wellness->Text
showgroomed wellR = if (wellnessGroomed wellR) then "Y" else "-"

staticFiles "static"
               

data App = App
  { httpManager :: Manager
    ,connection :: ConnectionPool
    ,democonnection::ConnectionPool
    ,getStatic::Static
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
  isAuthorized (VetVisitR _ _) _ = isAdmin
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

-- DB


instance YesodPersist App where
  type YesodPersistBackend App = SqlPersistT
  runDB db = do
    mname <- maybeAuthId
    App manager pool pool2 static <- getYesod
    let thepool = case mname of
          Just "demo" -> pool2
          _      -> pool
    runSqlPool db thepool



-- authenication
isAdmin::HandlerT App IO AuthResult
isAdmin = do
    usrsMap <- liftIO  getUsrs
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> Unauthorized "You must log in to an authorized account" --AuthenticationRequired
        Just usr -> if (Map.member usr usrsMap) then Authorized
          else  Unauthorized "Your account is  not authorized"

--  HTML  CSS ELEMENTS
cssmenuWidget::Bool->Widget
cssmenuWidget mode = $(widgetFileNoReload def "cssmenu")

mainMenu::Bool->Widget
mainMenu mode= do
          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
          cssmenuWidget mode

nameWidget::Widget
nameWidget = $(widgetFileNoReload def "nameC")

-- header
        
headerLogWid imgpath maid = do
         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
         addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
         addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"
         $(widgetFileNoReload def "headerLog")
headerwidget imgpath = $(widgetFileNoReload def "header")


--  TIME ROUTINES 
doparseTime::String->Day
doparseTime  st = readTime defaultTimeLocale "%-m/%-d/%-Y" st

showtime::Day->Text
showtime time = pack (formatTime defaultTimeLocale "%m/%d/%Y" time)

showfiletime time = (formatTime defaultTimeLocale "%m_%d_%Y" time)

date2textM Nothing = Nothing
date2TextM (Just day) = Just (showtime day)

date2text day = showtime day;

text2date::FormResult Text -> FormResult Day
text2date  =  fmap (doparseTime.unpack) 

text2dateM::FormResult (Maybe Text)->FormResult (Maybe Day)
text2dateM = fmap (fmap (doparseTime.unpack)) 

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
-- canbutton::Widget
-- canbutton =  $(widgetFileNoReload def "cancelButton")
-- AUTHORIZATION
     
instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [authBunnyluv]

    authHttpManager = httpManager

    maybeAuthId = lookupSession "_ID"

authBunnyluv :: YesodAuth m => AuthPlugin m
authBunnyluv =
    AuthPlugin "bunnyluv" dispatch login
  where
    dispatch "POST" [] = do
        ident <- lift $ runInputPost $ ireq textField "ident"
        usrsMap <-  liftIO  getUsrs
        let isOnFile = Map.member ident usrsMap
        let upass = if isOnFile then usrsMap Map.! ident else "guest"
        pass <-lift $ runInputPost $ ireq passwordField "pass"
        if (pass == upass)   then
         lift $ setCredsRedirect $ Creds "bunnyluv" ident []
        else loginErrorMessageI  LoginR PassMismatch

    dispatch _ _ = notFound
    url = PluginR "bunnyluv" []
    login authToMaster = do
        imgpath<- liftIO getImagePath
        msg <- getMessage
        headerwidget (unpack imgpath)
        $(widgetFileNoReload def "cancelButton")
        toWidget 
           [whamlet|
$newline never
 <div #logTitle style="float:inherit; margin:10px;">
    <b> BunnyLuv Login
    <div .cancelBut #rabEdCan style="display:inline; float:right;">
          <a href="/"> cancel </a>

<form method="post" action="@{authToMaster url}">
    <div>
      You must have a valid account to login to the Rabbit Tracker
    <div>
      If you need an account an adminstrator must create one for you.

    <div #login>
    <p>Username: #
     <input type="text" name="ident">
    <p>Password: #
     <input type="password" name="pass">
    <p>
    <input type="submit" value="Login">
    $maybe tmsg <-msg
       <p> #{ tmsg}
    <div>
      To access the training/demo Rabbit Tracker as an administrator login with
    <div>
      user "demo" and password "demo".  
|]

usrInsert umap (Entity uId (Usr nme pss))  = Map.insert nme pss umap

getUsrsDb::IO [Entity Usr]
getUsrsDb = runSqlite usrsDB $ do
    tusrs <- select $ from $ \person-> do
      return person
    return tusrs

getUsrs  :: IO (UsrMap)
getUsrs  = do
    tusrs <-  getUsrsDb
    return $  foldl (\accum eusr->usrInsert accum eusr) Map.empty tusrs
    
                                     

-- Database init


openConnectionCount :: Int
openConnectionCount = 10



initDB::IO ()
initDB = do
  ct<-getCurrentTime
  tz<-getCurrentTimeZone
  let local_time = utcToLocalTime tz ct
  withSqlitePool bunnyLuvDB openConnectionCount $ \pool -> do
   runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll

        insert $ Usr "sharon" "bunnyluv"
        insert $ Usr "stuart" "jrr1jrr1"
        
{-
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
-}
        return ()
