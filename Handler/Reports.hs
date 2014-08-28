{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, QuasiQuotes #-}

module Handler.Reports where

--this is a test 

--import qualified Data.ByteString.Lazy as L
import Conduit

--import Data.Conduit
--import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update, (>=.), (<=.))
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack, append)
import Database.Esqueleto
--import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
--import Database.Persist.Sql (insert)
--import Control.Monad.IO.Class (liftIO)
--import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import AgeForm
import Utils
import FormUtils
import qualified Data.Map as M

--getBonded::Handler ([Rabbit, Bonded, Rabbit])
getBonded::Handler [(Entity Rabbit, Entity Bonded, Entity Rabbit)]

getBonded = runDB $
   select $
   from $ \( rab1 `LeftOuterJoin` bonded `InnerJoin` rab2) -> do
   on (rab2 ^. RabbitId ==. bonded ^. BondedSecond) 
   on (rab1 ^.RabbitId ==. bonded ^. BondedFirst)
   groupBy (bonded ^. BondedRelation)
   orderBy [desc (rab1 ^. RabbitName)]
   return (rab1, bonded, rab2)

getBonded2::Handler [(Entity Rabbit, Entity Bonded, Entity Rabbit)]
getBonded2 = runDB $
   select $
   from $ \( rab1, bonded,  rab2) -> do
   where_ ((rab2 ^. RabbitId ==. bonded ^. BondedSecond) &&. (rab1 ^. RabbitId ==. bonded ^.BondedFirst))
   orderBy [asc (rab1 ^. RabbitName), asc (bonded ^. BondedRelation)]
   return (rab1, bonded, rab2)

data NId = NId {
           rname::Text
           ,rid::RabbitId
            } deriving (Show, Eq, Ord)
                       
type BondedMap = M.Map NId (M.Map Text [NId])

type BondedSet = (Entity Rabbit, Entity Bonded, Entity Rabbit)

getRelM rel | rel=="Friend" = rel
           | otherwise= "Family"

getInsert rel name@(NId rn rid) | rel== "Friend" = name
                   | otherwise= NId ( append rn (append " (" (append rel ") "))) rid
                                
add2map::BondedMap->BondedSet->BondedMap
add2map map (Entity rabId1 rab1, Entity bondId (Bonded r1 r2 rel), Entity rabId2 rab2) = theMap where
     name = NId (rabbitName rab1) rabId1
     n2 = NId (rabbitName rab2) rabId2
     relMap = M.lookup name map
     newRel = case relMap of
        Nothing->M.insert (getRelM rel) [getInsert rel n2] M.empty
        Just amap->M.insertWith' (++) (getRelM rel) [getInsert rel n2] amap   
     theMap =M.insert name  newRel map

makeMap::[BondedSet]->BondedMap
makeMap = foldl (\acc el-> add2map acc el) M.empty


isProcedureAlter::VetVisit->Bool
isProcedureAlter vvR = tval where
  proce = vetVisitProcedures vvR
  tval | proce =="Spayed" = True
       | proce =="Neutered" = True
       | otherwise = False


data Display = Display {
           disStartDate:: Day
          ,disEndDate:: Day
          ,disBunnyluv::Maybe Bool
           } deriving Show
                      
displayForm::Day->Day->Maybe Bool->Html->MForm Handler (FormResult Display, Widget)
displayForm begin end bl extra = do
    local_time <- liftIO  getLocalTime
    let today = localDay local_time
    let startTime = addDays (-30) today
    let stime = showtime end
    let ttime = showtime begin
    
    (startDateRes, startDateView) <- mreq textField (field "astart" "startD") (Just ttime)
    (endDateRes, endDateView) <- mreq textField (field "aend" "endD") (Just stime)
    (iBlRes, iBlView) <- mopt checkBoxField (field "abl" "blD")  (Just bl)
    let tstartDate = text2date startDateRes
    let tendDate =   text2date endDateRes
    let displayRes= Display <$> tstartDate <*> tendDate <*> iBlRes
    let wid1 =[whamlet| #{extra}
           <div #dispForm>
               <div #endDateB>
                 <div .bllabel> From #
                 <div #sdateIn .blDate> ^{fvInput startDateView}
               <div #startDateB>
                 <div .bllable> to #
                 <div #edateIn .blDate> ^{fvInput endDateView}
               $maybe bluv <-bl
                <div #inBL>
                   <div .bllabel>Only BunnLuv
                   <div #blIn> ^{fvInput iBlView}
            <input type=submit value="search">
               |]
    let wid2= toWidget [lucius|
          #dispForm div {
               display:inline-block;
               float:left;
               margin-right:5px;
            }

          #dispForm input {
               display:inline;
               float:right;
           }

          ##{fvId startDateView}, ##{fvId endDateView} {
              width:6em;
           }
         
          .bllabel {
             font-size:95%;
           }
 |]
    let wid = do
           wid1
           wid2
    return (displayRes, wid)

displayFormWidget wid enc link= do
        [whamlet| 
                        <form #dispRange method=post action=@{link} enctype=#{enc}>
                            ^{wid}
                            |]
        toWidget [lucius|
                  #dispRange {
                    padding:4px;
                 }
|]
                            
reportbase begin end bl link atitle result = do 
     (formWidget, enctype) <- generateFormPost getNameForm
     (ageWidget, age_enctype) <-generateFormPost getAgeForm
     (rangeWidget, range_enctype)<-generateFormPost (displayForm begin end bl)
     bnames <-  getNamesDB
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     let mode = (maid == Just "demo")
     let isAuth=(auth==Authorized)
     defaultLayout $ do
        setTitle atitle
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
        toWidget [julius| $( document ).ready(function(){
                             if (#{isAuth}) { 
                              $( "#cssmenu li:eq(1)" ).show(); 
                              $( "#blAdmin" ).show(); }                                                       else {
                              $( "#cssmenu li:eq(1)" ).hide(); 
                              $( "#blAdmin" ).hide(); }

                           });


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
        <div #blHeaderD>
        ^{getNameWidget bnames formWidget enctype}
         ^{headerLogWid imgpath maid}
         ^{mainMenu mode}
         ^{getAgeWidget ageWidget age_enctype}
        ^{displayFormWidget rangeWidget range_enctype link}
        <div #dateError>
              Invalid Date!
        <div #formResults>
         <div #atitleD> 
              <b> #{atitle} 
         ^{result}
         |]
        $(widgetFileNoReload def "nameC")
        toWidget [lucius| #atitleD {
                                width:100%;
                                float:left;
                                text-align:center;
                                background:#e8e8e8;
                                padding-bottom:5px;
                                padding-top:5px;
                                border-bottom:thin solid #404040;
                        }

                       #dateError {
                            position:absolute;
                            background:#f8f8f8;
                            color:#ff0000;
                            border:1px solid #7f0000;
                            box-shadow:1px 1px 3px #7f7f7f;
                            margin-left:100px;
                            padding:4px;
                            padding-left:6px;
                            padding-right:6px;
                            display:none;
                            transform:translateY(-105px);
                            z-index:200;
                          }
                            
              |]
 
twid drr = do
          [whamlet| 
           $forall (Entity drId dr) <- drr
             <div .blockDR >
               <div .rDR >
                  <div #dateDR style="float:left;">
                     #{showtime (dailyReportDate dr)}
                  <div #personDR style="float:right;">
                     #{dailyReportPerson dr}
               <div .rDR>
                    #{dailyReportReport dr}
            |]
          toWidget [lucius|
                   .blockDR {
                       border-bottom:1px solid #7f7f7f;
                       border-top:1px solid #7f7f7f;
                       width:100%;
                       float:left;
                     }

                   .rDR {
                       margin:5px;
                       margin-right:10px;
                       width:100%;
                       display:block;
                       float:left;
                     }
            |]

dailyView begin end  =  runDB $ select $
               from $ \tdr-> do
               where_ (( tdr ^. DailyReportDate >=. val begin) &&. (tdr ^. DailyReportDate <=. val end))
               return tdr


postDailyViewR::Handler Html
postDailyViewR = do
   (today, begin)<-getDefaultdays
   ((res, _), _) <- runFormPost (displayForm today begin (Just True))
   case res of
     FormSuccess (Display start end bl)->do
       drr <- dailyView start end
       reportbase start end Nothing DailyViewR "Daily Report" (twid drr)
     _ -> redirect DailyViewR

getDailyViewR::Handler Html
getDailyViewR = do
  (today, begin)<-getDefaultdays
  drr<- dailyView begin today
  reportbase begin today Nothing  DailyViewR "Daily Report" (twid drr)
                   
  
getDefaultdays = do
        today<- liftIO  getCurrentDay
        let begin = addDays (-30) today
        return (today, begin)



doAdoptedReport begin end = runDB $ 
  select $ from $ \(tr, tvv)-> do
    where_ ((tvv ^. AdoptedRabbit ==. tr ^. RabbitId) &&. (tvv ^. AdoptedDate >=. val begin) &&. (tvv ^. AdoptedDate <=. val end))
    orderBy [desc ( tvv ^. AdoptedDate)]
    return (tr, tvv)
  
adoptedReport adoptReport = $(widgetFileNoReload def "adoptedReport")

postAdoptedViewR::Handler Html
postAdoptedViewR = do
   (today, begin)<-getDefaultdays
   ((res, _), _) <- runFormPost (displayForm today begin (Just True))
   case res of
     FormSuccess (Display start end bl)->do
       aReport <- doAdoptedReport start end
       reportbase start end Nothing AdoptedViewR "Adopted Report" (adoptedReport aReport)
     _ -> redirect AdoptedViewR

getAdoptedViewR::Handler Html
getAdoptedViewR   = do
    (today, begin)<-getDefaultdays
    aReport <-doAdoptedReport begin today
    reportbase begin today Nothing AdoptedViewR "Adopted Report" (adoptedReport aReport)

           
doAReport idField dateField begin end bl = runDB $ do
  let blt = case bl of
               Just js->js
               Nothing->False
               
  select $ from $ \(tr, tvv)-> do
    let bstr= if blt then
              (tr ^. RabbitStatus ==. val "BunnyLuv")
             else
              (tr ^. RabbitStatus `like` (%))
    where_ ((tvv ^. idField ==. tr ^. RabbitId) &&. (tvv ^. dateField >=. val begin)
             &&. (tvv ^. dateField <=. val end) &&. bstr)
    orderBy [desc ( tvv ^. dateField)]
    return (tr, tvv)
    
doWellnessReport begin end bl = runDB $ do
  select $ from $ \(tr, tvv)-> do
    let bstr= if bl then
              (tr ^. RabbitStatus ==. val "BunnyLuv")
             else
              (tr ^. RabbitStatus `like` (%))
    where_ ((tvv ^. WellnessRabbit ==. tr ^. RabbitId) &&. (tvv ^. WellnessDate >=. val begin)
             &&. (tvv ^. WellnessDate <=. val end) &&. bstr)
    orderBy [desc ( tvv ^.WellnessDate)]
    return (tr, tvv)
  
weReport wellReport = $(widgetFileNoReload def "wellnessReport")

postWellViewR::Handler Html
postWellViewR = do
   (today, begin)<-getDefaultdays
   ((res, _), _) <- runFormPost (displayForm today begin (Just True))
   case res of
     FormSuccess (Display start end bl)->do
       wellReport <- doAReport WellnessRabbit WellnessDate start end bl
       reportbase start end bl WellViewR "Wellness Report" (weReport wellReport)
     _ -> redirect WellViewR


getWellViewR::Handler Html
getWellViewR   = do
   (today, begin)<-getDefaultdays
   wellReport <-doWellnessReport begin today  True
   reportbase begin today (Just True)  WellViewR "Wellness Report" (weReport wellReport)

{-
doVetVisits = runDB $ 
  select $ from $ \(tr, tvv)-> do
    where_ (tvv ^. VetVisitRabbit ==. tr ^. RabbitId)
    orderBy [desc ( tvv ^. VetVisitDate)]
    return (tr, tvv)
-}  
vvReport vetVisits = $(widgetFileNoReload def "vetvisitReport")

postVVViewR::Handler Html
postVVViewR = do
   (today, begin)<-getDefaultdays
   ((res, _), _) <- runFormPost (displayForm today begin (Just True))
   case res of
     FormSuccess (Display start end bl)->do
       vetvisits <-doAReport VetVisitRabbit VetVisitDate start end bl
       reportbase start end bl VVViewR "Vet Visits" (vvReport vetvisits)
     _ -> redirect VVViewR

getVVViewR::Handler Html
getVVViewR   = do
  (today, begin)<-getDefaultdays
  vetvisits <-doAReport VetVisitRabbit VetVisitDate begin today (Just True)
  reportbase begin today (Just True) VVViewR "Vet Visits" (vvReport vetvisits)

treatmentReport treatments = $(widgetFileNoReload def "TreatmentsReport");

getTreatmentReportR::Handler Html
getTreatmentReportR = do
  (today, begin)<-getDefaultdays
  treatments <- doAReport TreatmentBRabbit TreatmentBStart begin today (Just True)
  reportbase begin today (Just True) TreatmentReportR "Treatments" (treatmentReport treatments)
  
postTreatmentReportR::Handler Html
postTreatmentReportR = do
   (today, begin)<-getDefaultdays
   ((res, _), _) <- runFormPost (displayForm today begin (Just True))
   case res of
     FormSuccess (Display start end bl)->do
       treatments <-doAReport TreatmentBRabbit TreatmentBStart start end bl
       reportbase start end bl TreatmentReportR "Treatments" (treatmentReport treatments)
     _ -> redirect TreatmentReportR
getRel map nm rid = M.keys (map M.! (NId nm rid))

getRabs map nm rel rid = (map M.! (NId nm rid)) M.! rel


getBondedViewR::Handler Html
getBondedViewR = do
  bonded<- getBonded2
  let bmap = makeMap bonded
  (formWidget, enctype) <- generateFormPost getNameForm
  (ageWidget, age_enctype) <-generateFormPost getAgeForm
  bnames <-  getNamesDB
  msg <-getMessage
  maid <- maybeAuthId
  auth <- isAdmin
  impath <- liftIO getImagePath
  let imgpath = unpack impath
  let mode = (maid == Just "demo")
  let isAuth=(auth==Authorized)
  defaultLayout $ do
    setTitle "Friends and Family"
    [whamlet|
     <div #blHeaderD>
        ^{getNameWidget bnames formWidget enctype}
         ^{headerLogWid imgpath maid}
         ^{mainMenu mode}
         ^{getAgeWidget ageWidget age_enctype}
        <div #atitleD>
          <b> Friends and Family
     $forall NId rabName rid <- M.keys bmap
        <div #ffblock style="border-bottom:1px solid;">
           <b> <a href=@{ViewR (rid)}> #{ rabName}</a> </b>
           $forall rel <- getRel bmap rabName rid
              <div #relblock style="border-top:1px dashed #7f7f7f;">
                #{rel}: #
                 $forall NId rn ri<- getRabs bmap rabName rel rid
                      <a href=@{ViewR ri} style="margin-left:5px;"> #{rn} </a>#
              
     |]
    toWidget [lucius| #atitleD {
                                width:100%;
                                float:left;
                                text-align:center;
                                background:#e8e8e8;
                                padding-bottom:5px;
                                padding-top:5px;
                                border-bottom:thin solid #404040;
                        }

                     #ffblock a {
                          color:#000000;
                      }
                     #ffblock a:hover {
                          color:#6f6f6f;
                      }
       |]
     
