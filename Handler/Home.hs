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
import AgeForm

notBond rab arg = arg $
           from $ \bonded -> 
           where_  ((bonded ^. BondedFirst) ==. (rab ^. RabbitId))

notSelected rab = notExists $
           from $ \adopt -> do
           where_ (rab ^. RabbitId ==. adopt ^. AdoptRab)
           
queryCompanion male female hasff noff = do
    let mstr=if male then "M" else "Z"
    let fstr=if female then "F" else "Z"
    case (hasff, noff) of
      (True, True)->    runDB $ do
                         select $ from $ \rab-> do
                           where_ ((rab ^. RabbitStatus ==. val "BunnyLuv") &&.
                             ((rab ^. RabbitSex ==. val mstr) ||. (rab ^. RabbitSex ==. val fstr)) &&.
                               (notSelected rab)

                               )
                           return rab
      (True, False)->    runDB $ do
                         select $ from $ \rab-> do
                           where_ ((rab ^. RabbitStatus ==. val "BunnyLuv") &&.
                             ((rab ^. RabbitSex ==. val mstr) ||. (rab ^. RabbitSex ==. val fstr)) &&.
                               (notBond rab exists) &&. (notSelected rab))
                           return rab
      (False, True)->    runDB $ do
                         select $ from $ \rab-> do
                           where_ ((rab ^. RabbitStatus ==. val "BunnyLuv") &&.
                             ((rab ^. RabbitSex ==. val mstr) ||. (rab ^. RabbitSex ==. val fstr)) &&.
                               (notBond rab notExists) &&. (notSelected rab))
                           return rab
      (False, False)-> return []

getAges yrs diffMnths male female hasff noff= do
    b1 <-queryCompanion male female hasff noff
    today <- liftIO getCurrentDay
    let bday = addDays (yrs*(-365)) today
    let mnthDays = 31*diffMnths
    let result = sortEnt mnthDays bday b1
    let ageTit= "Rabbits within " ++ (show diffMnths) ++ " months of age "++(show yrs)
    base "Companion Rabbit Results" (toHtml ageTit) result



postGetAgeR::Handler Html
postGetAgeR  = do
  ((result, _), _) <- runFormPost getAgeForm
  case result of
       FormSuccess (AgeSearch age ageDiffMnths (Just male) (Just female) (Just ff) (Just noff)) ->getAges age ageDiffMnths male female ff noff
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

{-
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
-}

getAlteredR isAlt = do
     zinc<- queryAltered isAlt
     let ti = if (isAlt=="No") then "Not Altered" else "Altered"
     base "Altered" ti  zinc

getAllR = do
    bl <-queryStatus "BunnyLuv"
    ad <-queryStatus "Adopted"
    di <-queryStatus "Died"
    eu <-queryStatus "Euthanized"
    let zinc = bl++ad++di++eu
    base "All Rabbits" "All Rabbits" zinc
  
getQueryR status  = do
     zinc<- queryStatus status
     let ti = append "Status: " status
     base (toHtml ti) (toHtml ti) zinc
     
getSourceR source  = do
    zinc<- querySource source
    let ti = append "Source: " source
    base (toHtml ti) (toHtml ti)  zinc

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
    base (toHtml ti)(toHtml ti) zinc
  return page
   

postNameR::Text->Handler Html 
postNameR task= do
  ((result,_), _) <- runFormPost getNameForm
  case result of
    FormSuccess name -> case task of
                             "Wellness"->do
                                      names<-queryName name
                                      let id = getrabId (Prelude.head names)
                                      redirect (WellnessR id)
                             "Treatment"->do
                                      names<-queryName name
                                      let id = getrabId (Prelude.head names)
                                      redirect (TreatmentR id)
                             "Vet_Visit"->do
                                      names<-queryName name
                                      let id = getrabId (Prelude.head names)
                                      redirect (VetVisitR id "Other")
                             _  -> redirect (ShowNameR name)
                                      
    _ -> redirect HomeR



getHomeR :: Handler Html
getHomeR = do
    zinc <-queryStatus "BunnyLuv"
    base "BunnyLuv Rabbits" "BunnyLuv Rabbits" zinc

ageDiff::Day->Entity Rabbit->(Integer, Entity Rabbit)
ageDiff bday rabE@(Entity _ rab) = ( abs (diffDays bday (rabbitBirthday rab)), rabE)

clean::Integer->[(Integer, Entity Rabbit)]->[(Integer, Entity Rabbit)]
clean tageDiff = filter (\(a,_)-> a <= tageDiff) 

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
        sorted =  sortBy sortImp cleaned
        nrabs = map extractRabb sorted
  
getAgesR::Integer->Handler Html
getAgesR yrs = do
    b1 <-queryStatus "BunnyLuv"
    today <- liftIO getCurrentDay
    let bday = addDays (yrs*(-365)) today
    let result = sortEnt ageDiffMax bday b1
    let ageTit= "Rabbits within 2 years of age "++(show yrs)
    base "Rabbit Age" (toHtml ageTit) result

ffWid::RabbitId->Widget
ffWid rId = do
  family <- handlerToWidget (queryGetFamily rId)
  friends <- handlerToWidget (queryGetFriends rId)
  let hasfam = not (null family)
  let hasfriends = not (null friends)
  [whamlet|
    <#bondedBlock>
      <div #vrFriends  >
       $if hasfriends
         <div .bllabel> Friends: #
          $forall (Entity rabId rabb, Entity bId (Bonded r1 r2 relation)) <-friends
            \ <a class="rabLink" href="##{rabbitName rabb}"> #{rabbitName rabb} &nbsp;</a>
      <div #fam>
        $if hasfam
          <div .bllabel> Family: #
           $forall (Entity rabId rabb, Entity bId (Bonded r1 r2 relation)) <- family 
               \ <a class="rabLink" href="##{rabbitName rabb}"> #{rabbitName rabb} (#{relation})  &nbsp; </a> 
  |]
  toWidget [lucius|
       .bllabel {
         padding-left:5px;
         padding-right:5px;
       }
       #bondedBlock {
         float:left;
        }
       #bondedBlock div {
            font-size:95%;
       }
       #vrFriends, #fam {
          width:97%;
        }         
    |]


getAdoptableR::Handler Html
getAdoptableR = do
     avail<-getAdoptAvailableRabs
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     let mode =  (maid == Just "demo")
     let isAuth=(auth==Authorized)
     today<- liftIO getCurrentDay
     defaultLayout $ do
      addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
      addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"

      [whamlet|
     <div #thePage>
      $forall (Entity rId rab, rabstoryM) <-avail
  
          <div .rabBlock >
           <a .rabTarget ##{rabbitName rab}>
            $maybe img <- rabbitImage rab
             <div #imgBlock style="background-image:url('#{mkLink img imgpath}');">
            $nothing 
             <div #imgBlock style="background-image:url('#{mkLink "bunnyluvWide.jpg" imgpath}');">

           <div #story>
             <div #nameLine style="width:100%;">
              <div #rName>
                <b> #{rabbitName rab}
              <div #rAge>
                  #{getCurrentYears today rab} yr #{getCurrentMonths today rab} mnth
             <div #stry>
               $maybe (Entity sId (RabbitStory rId rstory))<- rabstoryM
                 #{rstory}
               $nothing
                   #{rabbitName rab} would like a good home.
             <div #ff>
             ^{ffWid rId}
        |]
      toWidget [julius|
  $(function () {
      var last="empty"
     $( ".rabLink" ).click( function (e) {
         var theval = $( this ).attr("href");
         $( theval ).parent().css("border-color", "yellow");
         if (last != "empty") {
            $( last ).parent().css("border-color", "transparent");
          }
         last= theval;        
      });
     });

  $( function () {
     $( window ).resize( function () {
       reSizeBlocks();
     });
    });

  var brblock=0;
  var imgblockH=0;
  var rblockH=0;

  function reSizeBlocks() {
     wwidth = $( "#thePage" ).width()-10;
     var rblock = brblock+7;
     var tiles = wwidth/rblock;
     var tint = Math.floor(tiles);
     var frac = tiles - Math.floor(tiles);
     var marg= Math.floor((frac*rblock)/tint);
     var nblock =0;
     var blockH= 0;
     if (marg > 18) {
          var nblock = marg-18;
          blockH= Math.floor(nblock/2);
          marg = 18; 
      } 
     $( ".rabBlock" ).each(function (index) {
           $( this ).css("margin-left", (marg+"px"));
           $( this ).css("width",(( brblock+nblock) + "px"));
           $( this ).css("height", ((rblockH+blockH) + "px"));
           $( this ).find( "#imgBlock" ).css("height", ((imgblockH+blockH)+"px"));
        });    
    }
  $(function () {
      brblock = $( ".rabBlock" ).width();
      imgblockH = $( "#imgBlock" ).height();
      rblockH = $( ".rabBlock" ).height();

     reSizeBlocks();
   });
 |]
      toWidget [lucius|
     body {
      background:#efefef;
     }
     #thePage div {
          float:left;
      }
      #rName,#rAge {
         margin-left:10px;
        }
     #rAge, #stry {
       font-size:90%;
      }
     #stry {
       margin-left:5px;
       margin-right:5px;
       margin-bottom:3px;
       width:97%;
       
      }
     #story {
       width:100%;
      }
     
     #thePage #rAge {
         float:right;
         padding-right:5px;
      }

     #thePage img {
             width:95%;
             float:left;
             margin:10px;
            }
        .rabBlock {
            float:left;
            width:290px;
            height:375px;
            box-shadow:2px 2px 4px;
            margin-top:15px;
            background:#fbfbfb;
            border:3px solid transparent;
           }

  #imgBlock {
    float:left;
    display: inline-block;
    width: 98%;
    height:66%;
    margin: 4px;
    border: 1px solid black;
    background-position: center center;
    background-size: cover;

  }
  

           
                |]
