{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module FormUtils where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar

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

personForm:: MForm Handler (FormResult Person, Widget)
personForm   = do
    (personFirstNameRes, personFirstNameView)<-mreq textField "nope" Nothing
    (personLastNameRes, personLastNameView)<-mreq textField "nope" Nothing
    (personPhoneRes, personPhoneView)<-mreq textField "nope" Nothing
    (personMobileRes, personMobileView)<-mopt textField "noop" Nothing
    (personStreetRes, personStreetView)<-mreq textField "nope" Nothing
    (personAptRes, personAptView)<-mopt textField "noop" Nothing
    (personCityRes, personCityView)<-mreq textField "nope" Nothing
    (personStateRes,personStateView)<-mreq textField "nope" Nothing
    (personZipRes, personZipView)<-mreq textField "nope" Nothing
    (personEmailRes, personEmailView)<-mopt textField "nope" Nothing
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
    toWidget [julius|

   function checkDate( astr ) {
      var str =astr; 
      var pat=/^([1-9]|0[1-9]|1[012])[/]([1-9]|0[1-9]|[12][0-9]|3[01])[/]((19|20)[0-9][0-9]|[0-9][0-9])$/;
    var res = str.match(pat);
    if (res==null) {
      return "";
    }
    
    var dtMonth=res[1];
    var dtDay=res[2];
    var adtYear=res[3];
    if (adtYear.length == 2) 
         {dtYear="20"+adtYear;} 
    else {dtYear=adtYear;}

    if ((dtMonth==4 || dtMonth==6 || dtMonth==9 || dtMonth==11) && dtDay ==31) {
       return "";
    }

  if (dtMonth == 2)
     {
     var isleap = (dtYear % 4 == 0 && (dtYear % 100 != 0 || dtYear % 400 == 0));
     if (dtDay> 29 || (dtDay ==29 && !isleap)) {
          return "";
        }
     }
  return (dtMonth +"/"+ dtDay+"/"+dtYear);
    
  }
            $(function () {
              $( ".blDate :input" ).blur ( function(e) {
                 var str = $( this ).val();
                 if (str.length < 1)
                   return;
                 var thedate = checkDate( str );
                 if (thedate.length < 4) {
                   e.preventDefault();
                   $( this ).val( "" );
                    $( "#dateError" ).show();
                   $( this ).focus();              
                  } else {
                  $( this ).val ( thedate );
                  $( this ).change();
                 }
                });
               });

            $(function () {
              $( ".blDate :input" ).keydown ( function(e) {
                 $( "#dateError" ).hide();
               if (e.keyCode==13 || e.keyCode==9) {
                 var str = $( this ).val();
                 if (str.length < 1)
                   return;
                 var thedate = checkDate( str );
                 if (thedate.length < 4) {
                   e.preventDefault();
                   $( this ).val( "");
                    $( "#dateError" ).show();
                   $( this ).focus();
                  } else {
                 $( this ).val ( thedate );
                 $( this ).change();
                 }
                }
                });
               });


 
       |]
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
