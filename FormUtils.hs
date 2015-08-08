{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module FormUtils where

--this is a test 

--import qualified Data.ByteString.Lazy as L
import Conduit

--import Data.Conduit
--import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
--import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
--import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import System.FilePath


                 
topWidget::Widget->Widget
topWidget awid = do
  addStylesheetRemote "/static/css/baseAdoption.css"
  [whamlet|
   <div #banner>
       <img #bimage src=@{StaticR bunnyluvBanner_gif} style="width:100%;">
     <div #menu>
       <div #bl .md>
        <a href="http://www.bunnyluv.org">Home</a>
       <div #adoptable .md>
        <a  href=@{AdoptableR}><span>Adoptable Rabbits</span></a>
       <div #adoptAp .md>
        <a href="http://www.bunnyluv.com">Shop (Non-Profit)</a>
       <div .md>
        <a href=@{HomeR}>Tracker</a>
    ^{awid}
 
   |]
  toWidget [julius|
   $(document).ready(function(){
    $( ".md a" ).each(function(index) {
        if ( this.href.trim() == window.location )
            $( this ).parent().addClass( "selected" );
     });
  });
 
  |]
{-
  toWidget [lucius|

@media all {          
            .md.selected {
               border-top:2px solid red;
              }

             .md.selected a {
               color:red;
              }

            #banner {
              background:#fefefe;
              float:left;
              width:100%;
           }
             
             
            #menu {
              background:#fefefe;
              width:100%;
              float:left;
              border-bottom:1px solid #e0e0e0;
              }

           .md {
             height:27px;
             background:#FEFEFE;
             margin-right:10px;
             border-top:2px solid black;
             float:left;
             padding-top:10px;
             text-decoration:none;
             padding-left:10px;
             padding-right:10px;
             margin-bottom:5px;

          }

         .md a {
           text-decoration:none;
           color:#000000;
          }

         .md a:hover {
           color:#8f3f3f;
          }

         .md:hover {
           border-top: 2px solid #8f3f3f;
           background:#fffff8;
          }

}
@media screen and (min-resolution: 1.5dppx} {
         .md a {
         font-size:24px;
        }
}


            |]
-}
baseAdoption::Text->Widget->Widget->Handler Html
baseAdoption title blurb contentWid = 
  defaultLayout $ do
      addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
      addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"
      setTitle (toHtml title)

      [whamlet|
       ^{topWidget blurb}

       ^{contentWid}
     |]

getVets::Handler [Entity Vet]
getVets = runDB $ 
  select $ from $ \vet -> do
  where_ (vet ^. VetDoctor !=. val "")
  return vet
  
queryTreatmentB rabId = runDB $
  select $ from $ \t ->do
    where_ (t ^. TreatmentBRabbit ==. val rabId)
    orderBy [desc (t ^. TreatmentBStart)]
    return t

queryTreatmentBbyTreat treatId = runDB $
  select $ from $ \(treat, rab)->do
     where_ ((treat ^. TreatmentBId ==. val treatId) &&. ( rab ^. RabbitId ==. treat ^. TreatmentBRabbit))
     return (treat, rab)



field txt id = FieldSettings txt (Just txt) (Just id) (Just id) []

sourceType::[(Text,Text)]
sourceType=[("Shelter","Shelter"), ("Other", "Other")]

sex::[(Text,Text)]
sex =[("F","F"), ("M","M")]
altered::[(Text,Text)]
altered = [("No", "No"), ("Unknown", "Unknown"), ("Spayed", "Spayed"), ("Neutered", "Neutered")]
status::[(Text, Text)]
status = [ ("BunnyLuv", "BunnyLuv"), ("Adopted", "Adopted"), ("Died", "Died"), ("Euthanized", "Euthanized")]

vets::[(Text, Text)]
vets = [("Dr. Misetich", "Dr. Misetich"), ("Dr. Petritz", "Dr. Petritz"), ("Dr. Steele (C.A.R.E)", "Dr. Steele (C.A.R.E)")]

procedures::[(Text,Text)]
procedures=[("Spayed", "Spayed"), ("Neutered", "Neutered"), ("Euthanized", "Euthanized"), ("Other", "Other")]


writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    today<- liftIO  getCurrentDay
    uploadDir <- liftIO getUploadDir
    let date = showfiletime today
    let filename = unpack $ fileName file
        rf = reverse filename
        (ext, thead) = break (== '.') rf
        thead2 = tail thead
        fn = (reverse thead2) ++ "_" ++ date ++ "." ++ reverse ext
        path = imageFilePath uploadDir fn
    liftIO $ fileMove file path
    return fn

imageFilePath :: Text->String -> FilePath
imageFilePath adir f = unpack adir </> f

getPField Nothing _ = Nothing
getPField (Just per) tfield = Just (tfield per)

personForm:: Maybe Person->MForm Handler (FormResult Person, Widget)
personForm personM  = do
    (personFirstNameRes, personFirstNameView)<-mreq textField "nope" (getPField personM personFirstName)
    (personLastNameRes, personLastNameView)<-mreq textField "nope" (getPField personM personLastName)
    (personPhoneRes, personPhoneView)<-mreq textField "nope" (getPField personM personPhone)
    (personMobileRes, personMobileView)<-mopt textField "noop" (getPField personM personMobile)
    (personStreetRes, personStreetView)<-mreq textField "nope" (getPField personM personStreet)
    (personAptRes, personAptView)<-mopt textField "noop" (getPField personM personApt)
    (personCityRes, personCityView)<-mreq textField "nope" (getPField personM personCity)
    (personStateRes,personStateView)<-mreq textField "nope" (getPField personM personState)
    (personZipRes, personZipView)<-mreq textField "nope" (getPField personM personZip)
    (personEmailRes, personEmailView)<-mopt textField "nope" (getPField personM personEmail);
    let personRes = Person <$> personFirstNameRes <*>
                        personLastNameRes <*>  personPhoneRes <*>
                         personMobileRes <*> personStreetRes <*> personAptRes <*>
                         personCityRes <*> personStateRes <*> personZipRes <*>
                         personEmailRes 
    let pwid = $(widgetFileNoReload def "Person")
    return (personRes, pwid)


baseForm ti menu form = do
  maid <-maybeAuthId
  impath <- liftIO getImagePath
  let imgpath = unpack impath
  defaultLayout $ do
    setTitle ti
    $(widgetFileNoReload def "cancelbutton")
    $(widgetFileNoReload def "bldate");
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
    addScriptRemote "https://code.jquery.com/ui/1.11.0/jquery-ui.js"
    addStylesheetRemote "https://code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css" 
    [whamlet|
       <div #ablank style="color:#ffffff; float:right">  
                 This is a test
       ^{headerLogWid imgpath maid}
       ^{menu}
        <div #dateError> Error in Date Entry!
       ^{form}
        |]
    toWidget [lucius| 
          #dateError {
           position:absolute;
           color:#ff0000;
           background-color:#ffffff;
           margin-left:40%;
           border:1px solid #7f7f7f;
           box-shadow:2px 2px 3px #7f7f7f;
           display:none;
           padding:2px;
           padding-left:10px;
           padding-right:10px;
           margin-top:5px;
}
|]
