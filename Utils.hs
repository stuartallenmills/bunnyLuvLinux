{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Utils where

--this is a test 

--import qualified Data.ByteString.Lazy as L
--import Conduit

--import Data.Conduit
--import Data.Conduit.Binary
--import Data.Default
import Yesod hiding ((!=.), (==.), (||.))
--import Yesod.Default.Util
--import Yesod.Auth
import Foundation

import Data.Text (Text, unpack, append)
import Database.Esqueleto
--import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
--import Database.Persist.Sql (insert)
--import Control.Monad.IO.Class (liftIO)
--import Text.Printf
--import Data.Time
--import Data.List (sortBy)
import qualified Data.Text as T
import Text.Julius


gbp numE alist accum | length alist<numE = accum++alist
                     | otherwise = gbp numE l2 a2 where
                         (l1, l2) = splitAt numE alist
                         a2 = accum++l1
                         
groupByPage numEntries alist = gbp numEntries alist []

queryStory rabId = runDB $
 select $ from $ \story->do
   where_ (story ^. RabbitStoryRabbit ==. val rabId)
   return story
   

queryWellness rabId = runDB $ 
  select $ from $ \r ->do
     where_ (r ^. WellnessRabbit ==. val rabId)
     orderBy [desc (r ^. WellnessDate)]
     return r

queryVetVisits rabId = runDB $ 
 select $ from $ \r ->do
     where_ (r ^. VetVisitRabbit ==. val rabId)
     orderBy [desc (r ^. VetVisitDate)]
     return r

queryAdopted rabId = runDB $ 
 select $ from $ \r ->do
     where_ (r ^. AdoptedRabbit ==. val rabId)
     return r

queryTreatments::Handler [(Entity TreatmentB, Entity Rabbit)]
queryTreatments = runDB $
  select $ from $ \(treat, rab)->do
    where_ (rab ^. RabbitId ==. treat ^. TreatmentBRabbit)
    orderBy [desc (treat ^. TreatmentBStart)]
    return (treat, rab)

queryGetBonded rabId = runDB $ 
  select $
  from $ \(rab, bonded) -> do
  where_ ((bonded ^.BondedFirst ==. val rabId) &&. (rab ^. RabbitId ==. bonded ^. BondedSecond))
  return (rab, bonded)

queryGetFriends rabId  = runDB $ 
  select $
  from $ \(rab, bonded) -> do
  where_ ((bonded ^.BondedFirst ==. val rabId) &&. (rab ^. RabbitId ==. bonded ^. BondedSecond) &&.
              (bonded ^. BondedRelation ==. val "Friend"))
  return (rab, bonded)

queryGetFamily rabId  = runDB $ 
  select $
  from $ \(rab, bonded) -> do
  where_ ((bonded ^.BondedFirst ==. val rabId) &&. (rab ^. RabbitId ==. bonded ^. BondedSecond) &&.
              (bonded ^. BondedRelation !=. val "Friend"))
  return (rab, bonded)
  
queryStatus status = runDB $ 
 select $ from $ \r ->do
     where_ (r ^. RabbitStatus ==. val status)
     orderBy [asc (r ^. RabbitName)]
     return r

queryNotSelected::Handler [Entity Rabbit]
queryNotSelected = runDB $
   select $ from $ \rabs ->do
     where_ (notExists $
             from $ \adopt -> 
               where_ (rabs ^. RabbitId ==. adopt ^. AdoptRab))
     return rabs
  

queryName name = runDB $ do
  let (f,s) = T.splitAt 1 name
  let capName = T.append (T.toUpper f) s
  let lowName = T.append (T.toLower f) s
      
  select $ from $ \r ->do
     where_ ((like  (r ^. RabbitName)  ((%) ++. val capName ++. (%)) ) ||.
             (like  (r ^. RabbitName)  ((%) ++. val lowName ++. (%)) ) 
             )
     orderBy [asc (r ^. RabbitName)]
     return r

rabId2rab rabId = do
         Just rab <- runDB $ get rabId
         return rab
         
gostring::[T.Text]->T.Text
gostring alist = out where
           temp= foldl (\accum x-> T.append( T.append (T.append "\"" x) "\",") accum) T.empty alist
           out = T.append "["  (T.append (T.take ((T.length temp) - 1) temp) "]")

getrabId (Entity rId rab) = rId

getName::Entity Rabbit->Text
getName (Entity rabId rab) = rabbitName rab

getNames::[Entity Rabbit]->[Text]
getNames  = map getName 

getAdoptAvailable::Handler [Text]
getAdoptAvailable = do
  rabs<-runDB $
   select $ from $ \rabs ->do
     where_ ((rabs ^. RabbitStatus ==. val "BunnyLuv") &&. (notExists $
             from $ \adopt -> 
               where_ (rabs ^. RabbitId ==. adopt ^. AdoptRab)))
     return rabs
  return (getNames rabs)

getAdoptAvailableRabs::Handler [(Entity Rabbit, Maybe (Entity RabbitStory))]
getAdoptAvailableRabs = 
  runDB $
   select $ from $ \(rabs `LeftOuterJoin` story)->do
     on (just (rabs ^. RabbitId) ==. story ?. RabbitStoryRabbit)
     where_  ((rabs ^. RabbitStatus ==. val "BunnyLuv") &&.
                   (notExists $
             from $ \adopt -> 
               where_ (rabs ^. RabbitId ==. adopt ^. AdoptRab)))
       
     return (rabs, story)
  
getNamesDB:: Handler [Text]
getNamesDB = do
     rabs<-queryStatus "BunnyLuv"
     return (getNames rabs)

getNameForm = getNameFormB "formName"

getNameFormB::Text->Html->MForm Handler (FormResult Text, Widget)
getNameFormB id extra = do
  let fs = FieldSettings "sNamel" (Just "Find rabbit by name") (Just "getName") (Just "nameField") []
  (nameRes, nameView) <- mreq textField "default"  Nothing
  let wid =do
                  [whamlet| #{extra}
                     <div class="ui-widget getNameDiv" ##{id} style="font-size:1em; display:inline" title="Find rabbit by name">
                         <label style="font-size:0.8em;" for="getName" >name: </label>  ^{fvInput nameView}
                      <input #nameIn type=submit value="find" style="display:none;">                 
                      <div #berror> Not on file!
                    |]
                  toWidget [lucius|
                               ##{fvId nameView} {
                                      font-size:0.9em;
                                      width:15em;
                                  }
                             .ui-autocomplete {
                                   z-index:100;
                                 }
                             .ui-menu-item {
                                      font-size:0.8em;
                                }
                   #berror {
                     margin:5px;
                     font-size:95%;
                     color:#ef0000;
                     float:left;
                     display:none;
                     transform:translateY(-5px);
                   }

                      |]
                  
  return (nameRes, wid)

rightTopFormat::Widget
rightTopFormat =  toWidget [lucius|
                     @media screen {
                          #formName {
                              padding:0;
                              border:none;
                              margin:0;
                              float:right;
                             }
                           #formName input {
                              display:inline;
                           }
                         }
                     @media print {
                       #formName {
                          display:none;
                         }
                    }
            |]


newFormat::Text->Widget
newFormat task = toWidget [lucius|
                     @media screen {

                          ##{divName task} form {
                               border:none;
                               padding:0;
                             }
                          ##{divName task} {
                              padding:0;
                              border:1px solid #7f7f7f;
                              margin:1px;;
                              box-shadow:1px 1px 2px #7f7f7f;
                              display:none;
                              padding:10px;
                              transform: translate(100px, -50px);
                              width:220px;
                              z-index:100;
                              background:#f8f8f8;
                              position:absolute;
                             }
                           ##{divName task} input {
                              display:inline;
                           }
                         }
                     @media print {
                       ##{divName task}{
                          display:none;
                         }
                    }
            |]


divName = append "new" 
canName = append "cancel"
getTaskWidget bnames wid enctype task = [whamlet| 
                <div ##{divName task} .popup>
                 <div #newTaskTitle style="float:left; width=80%;" >
                   New #{task} for
                 <div .cancelBut .popupCancel ##{canName task} style="float:right;">cancel
                 ^{getNameWidgetG bnames wid enctype task (newFormat task) task}
                                      |]


getNameWidget::[Text]->Widget->Enctype-> Widget
getNameWidget bnames wid enctype = getNameWidgetG bnames wid enctype "formName" rightTopFormat  "bunn"

getNameWidgetG::[Text]->Widget->Enctype->Text->Widget->Text->Widget
getNameWidgetG bnames wid enctype form format taction= do
         format 
         [whamlet|
           <form ##{form} method=post action=@{NameR taction} enctype=#{enctype}>
            ^{wid}
           |]

         toWidget [julius|
                  $( document ).ready(function() { 
                    $( "##{rawJS form} :input" ).attr("title", "Find rabbit by name");
                    $( "##{rawJS form} :input" ).autocomplete({
                      search:"",
                      source: #{rawJS (gostring bnames)},
                      select: function (event, ui) {
                          $( this ).val (ui.item.label);
                          $( "##{rawJS form}" ).submit();
                        }
                    });
                   });

                  $(function() {
                      $( ".popupCancel" ).click( function() {
                        $( this ).parent().closest( "div" ).hide();
                        });
                       });

   function checkName ( inVal ) {
         var currval = inVal;
         var validOptions = #{rawJS (gostring bnames)};
         var capval = currval.charAt(0).toUpperCase()+ currval.slice(1);
         var amember = $.inArray( capval, validOptions );
            if ( amember > -1 ) {
                $( this ).val( capval );
                return capval;
            } else {
             return ("");
            }
        }

                           

               $(function() { $( "##{rawJS form} :input" ).keydown( function( e) {
                           $( "##{rawJS form} #berror" ).hide();
                           if (e.keyCode==13 || e.keyCode==9) {
                            var aname = $( this ).val();
                            var nname = checkName( aname );
                            if (nname.length < 1) {
                              e.preventDefault();
                              $( this ).val( "" );
                              $( this ).focus();
                              $( "##{rawJS form} #berror" ).show();
                              } else {
                              $( this ).val( nname );
                              }

                            }
                   });
                 });
                 


               |]
