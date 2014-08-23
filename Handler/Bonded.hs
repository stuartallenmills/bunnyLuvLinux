{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Bonded where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update, (/=.))
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
import Text.Julius
import FormUtils
import Utils

relates::[(Text, Text)]
relates=[("Friend", "Friend"),("Sibling", "Sibling"), ("Offspring", "Offspring"), ("Parent","Parent") ]

queryBondEntry r1 r2 =
  select $
  from $ \(bonded) -> do
  where_ ((bonded ^.BondedFirst ==. val r1) &&. ( bonded ^. BondedSecond ==.  r2))
  return bonded
  


queryGetNotBonded rabId = runDB $ 
  select $
  from $ \rab  -> do
  where_ $ (rab ^.RabbitId !=. val rabId) &&. ( notExists $
           from $ \bonded -> do
           where_ (((bonded ^. BondedFirst) ==. val rabId) &&. ((bonded ^. BondedSecond) ==. (rab ^. RabbitId))))
  return rab

getRel rel
        | rel=="Friend" = "Friend"
        | rel=="Sibling" = "Sibling"
        | rel=="Parent" = "Offspring"
        | rel=="Offspring" = "Parent"
        | otherwise = "Error"
                      
addBonded rabId1 rabId2 rel = runDB $ do
   insert (Bonded rabId1 rabId2 rel)
   insert (Bonded rabId2 rabId1 (getRel rel))
   return ()

getRabId (Entity rabId rabbit) = rabId

data BondedResult = BondedResult {
                           brRabId::RabbitId
                          ,brName::Text
                          ,brRelation::Text
} deriving Show

bondedForm::RabbitId->Html->MForm Handler (FormResult BondedResult, Widget)
bondedForm rabId extra = do
    let fs = FieldSettings "bName" (Just "Not friend/family") (Just "notBonded") (Just "notBondedField") []
    (bondRes, bondView) <- mreq textField fs  Nothing
    (relationRes, relationView) <-mreq (selectFieldList relates) "nope" Nothing
    let bondedRes = (BondedResult rabId) <$> bondRes <*> relationRes
    let bondedWid = do
          [whamlet| #{extra}
            <div #bonded>
                <div #bname>
                 <div #blN title="Only rabbits already on file can be added">
                    Name:
                 ^{fvInput bondView}
                 <div #berror>
                      \<- Rabbit not on file!
                <div #relation>
                  <div #bR>
                    Relation:
                  ^{fvInput relationView}
               <input type=submit value="submit" style="float:right;">

                    |]
          toWidget [lucius|
                 #bonded input {
                   display:inline;
                   float:left;
                  }

                  #bname, #relation {
                    width:100%;
                   }
                  #berror {
                     margin:5px;
                     font-size:95%;
                     color:#ef0000;
                     float:left;
                     display:none;
                     transform:translateY(-5px);
                   }

                  #blN, #bR {
                    width:12%;
                    float:left;
                   }
                  #bonded div {
                    margin-bottom:20px;
                    float:left;
                   }
                 ##{fvId relationView} {
                                width:10em;
                         }
                 |]
    return (bondedRes, bondedWid)


postBondedR::RabbitId->Handler Html
postBondedR rabId = do
  ((result, _), _) <- runFormPost (bondedForm rabId)
  case result of
    FormSuccess (BondedResult rabid rabname rel) -> do
      -- does not handle the case where multiple rabbits with same name
      rabs <- queryName rabname
      let (Entity rId arab) = Prelude.head rabs
      addBonded rabid rId rel
      redirect (BondedR rabId)
    _-> redirect (BondedR rabId)
      
  
getBondedR::RabbitId->Handler Html    
getBondedR rabId = do
  notbonded<-queryGetNotBonded rabId
  let notbtext = getNames notbonded
  bonded<-queryGetBonded rabId
  rab <- rabId2rab rabId
  (formWidget, enctype) <- generateFormPost (bondedForm rabId)
  let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">

                <b> Add Friends and Family for &nbsp; #{rabbitName rab}
                <div #vvCan style="float:right; display:inline;">
                  <div .cancelBut #vvEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabId}> cancel </a>
               |]
  let wWid =    [whamlet|
    <div #addff>
     <form  #bondedForm method=post action=@{BondedR rabId} enctype=#{enctype}>
       ^{formWidget}
    <div #cff>
       <div #tcff>
        Current friends and family:
       <div #clist>
        <div #fr>
         Friends: #
         $forall (Entity rabId rabb, Entity bId (Bonded r1 r2 relation)) <-bonded
           $if relation == "Friend"
            \  #{rabbitName rabb}  #
        <div #fam>
         Family: #
         $forall (Entity rabId rabb, Entity bId (Bonded r1 r2 relation)) <-bonded 
             $if relation /= "Friend"
              \  #{rabbitName rabb} (#{relation})  &nbsp;
            
     |]
  let jWid = toWidget [lucius| 
           #cff div {
                 margin-bottom:5px;
                }
           #cff #fam {
               margin-bottom:15px;
             }
           #cff #tcff {
               margin-top:15px;
           }
           .ui-menu-item {
                 font-size:0.8em;
              }

        |]
   
   
       
  let silly=  toWidget [julius|


    function checkName ( inVal ) {
         var currval = inVal;
         var validOptions = #{rawJS (gostring notbtext)};
         var capval = currval.charAt(0).toUpperCase()+ currval.slice(1);
         var amember = $.inArray( capval, validOptions );
            if ( amember > -1 ) {
                $( this ).val( capval );
                return capval;
            } else {
             return ("");
            }
        }



                   $( document ).ready(function() { 
                    $( "#notBonded" ).attr("title", "Find rabbit by name");
                    $( "#notBonded" ).autocomplete({
                      minLength:0,
                      source:#{rawJS (gostring notbtext)},
                      select: function (event, ui) {
                          $( this ).val (ui.item.label);
                          $( "#bondedForm" ).submit();
                        }
                     });
                   });
              
                $(function() { $( "#notBonded" ).blur( function( e) {
                             var aname = $( "#notBonded" ).val();
                            var nname = checkName( aname );
                            if (nname.length < 1) {
                               e.preventDefault();
                              $( "#notBonded" ).val( "" );
                              $( "#notBonded" ).focus();
                             $( "#berror" ).show();
                               
                             } else {
                              $( "#notBonded" ).val( nname );
                              }
                   });
                 });
                 
                $(function() { $( "#notBonded" ).keydown( function( e) {
                           $( "#berror" ).hide();
                           if (e.keyCode==13 || e.keyCode==9) {
                            var aname = $( "#notBonded" ).val();
                            var nname = checkName( aname );
                            if (nname.length < 1) {
                              e.preventDefault();
                              $( "#notBonded" ).val( "" );
                              $( "#notBonded" ).focus();
                              $( "#berror" ).show();
                              } else {
                              $( "#notBonded" ).val( nname );
                              }
                           }
                   });
                 });
                 
                 
               |]
  let bondedWid= do
          wWid
          jWid
          silly

  baseForm "Add Friends/Family" menu bondedWid
 
