{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.ViewAdoption where

import Conduit
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update, delete)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import FormUtils
import Utils
import Text.Julius

--getAdoptions::Handler [(Entity AdoptRequest, Entity Person)]
getAdoptions = runDB $
  select $ from $ \(adopt, per) -> do
    where_ ((adopt ^. AdoptRequestPerson) ==. (per ^. PersonId))
    return (adopt, per)

getPerson arId = runDB $ do
   req<- get arId
   case req of
     Just (AdoptRequest _ pid _ _)-> do
                                    per <- get pid
                                    return per
     _->return Nothing
     
                      
getSelectRecs reqId = runDB $
       select $ from $ \sel -> do
       where_ (sel ^. AdoptReq ==. val reqId)
       return (sel)
       
getRabs reqId = runDB $
  select $ from $ \(rabs, ad) ->do
    where_ ((ad ^. AdoptReq ==. val reqId) &&. (rabs ^. RabbitId ==. ad ^. AdoptRab))
    return (rabs)

getAdoptedRabs reqId = runDB $
  select $ from $ \(rabs, ad) ->do
    where_ ((ad ^. AdoptedRabsReq ==. val reqId) &&. (rabs ^. RabbitId ==. ad ^. AdoptedRabsRab))
    return (rabs)

getNotes reqId =  runDB $
   select $ from $ \notes->do
     where_ (notes ^. AdoptNotesAdoptRequest ==. val reqId)
     return notes

getAdoptForm id = runDB $ 
    select $ from $ \(adopt, per)-> do
     where_ ((adopt ^. AdoptRequestPerson ==. per ^. PersonId) &&. (adopt ^. AdoptRequestId ==. val id) )
     return (adopt, per)

data Notes = Notes {
               date::Day
               ,action::Textarea
                }


rabStatus::[(Text,Text)]
rabStatus =[("Selected", "Selected"), ("Adopted", "Adopted"), ("Cancel", "Cancel")]

data RabAdopt = RabAdopt {
  rabAdoptRequest::AdoptRequestId
  ,rabName::Text
  ,rabAdoptDate::Day
  } deriving Show

deleteRabbitForm::AdoptRequestId->Html->MForm Handler (FormResult RabAdopt, Widget)
deleteRabbitForm arId extra = do
  local_time <- liftIO  getLocalTime
  let stime = showtime (localDay local_time)
  (dateRes, dateView)<-mreq textField "nope" (Just stime)
  (rabbitRes,rabbitView)<-mreq textField (FieldSettings "nope" (Just "nope") (Just "rabName") (Just "rabName") []) Nothing
  let date = fmap (doparseTime.unpack) dateRes
  let res = RabAdopt arId <$> rabbitRes <*> date 
  let wid = do
       $(widgetFileNoReload def "cancelButton")
       per<- handlerToWidget ( getPerson arId) 
       rabs<- handlerToWidget (getRabs arId)
       let rabNames = getNames rabs
       [whamlet| #{extra}
          <div #anRForm>
             $maybe person <- per
              <div #reqPer style="float:left; width:100%;">
               <div #perName style="float:left">
                Remove Rabbit for  #{personFirstName person} #{personLastName person}
               <div .blDate #anDate style="float:left; padding-left:10%;">
                Date: ^{fvInput dateView}
               <div #dateError style="display:none; color:#ff0000; float:left">
                                     \<-Error
                                             
               <div .cancelBut #canBut, style="float:right; margin-left:20px;"> <a href=@{ViewAdoptForms}>cancel</a>
             <div #rabList>
               $forall Entity rId rabb <-rabs
                  <div #arab>
                    <div #rname style="padding-left:10px;"> #{rabbitName rabb}
             <div  #therab>
              <div .bllabel>
                Enter Rabbit to remove:
              <div #rabNameDel>
               ^{fvInput rabbitView}
               <div #berror>
                \<- Not Valid!

              <input .subButton  type=submit value="submit">
            |]
       toWidget [lucius|
             ##{fvId rabbitView} {
                  width:20em;
                  display:inline;
                  margin-left:10px;
                  float:left;
               }
             ##{fvId dateView} {
                  width:6em;
                  display:inline;
              }  
             
             #therab .bllabel {
                  float:left;
              }
             #rabNameDel {
               margin-top:10px;
             }
          |]

       toWidget [julius|
          $(function () {
          $( "#rname" ).click(function () {
            var val = $( this ).text();
            alert("click "+val);
          });
         });
         
   function checkDelName ( inVal ) {
         var currval = inVal;
         var validOptions = #{rawJS (gostring rabNames)};
         var capval = currval.charAt(0).toUpperCase()+ currval.slice(1);
         var amember = $.inArray( capval, validOptions );
            if ( amember > -1 ) {
                return capval;
            } else {
             return ("");
            }
        }

               $(function() { $( "#rabName" ).keydown( function( e) {
                           $( "#rabNameDel #berror" ).hide();
                           if (e.keyCode==13 || e.keyCode==9) {
                            var aname = $( this ).val();
                            var nname = checkDelName( aname );
                            if (nname.length < 1) {
                              e.preventDefault();
                              $( this ).val( "" );
                              $( this ).focus();
                              $( "#rabNameDel #berror" ).show();
                              } else {
                              $( this ).val( nname );
                              }

                            }
                   });
                 });

          
         $(function () {
           $( ".subButton" ).click( function (e) {
            var theval = $( "#rabNameDel :input").val();
            var res = checkDelName(theval);
            $( "#rabNameDel :input" ).val(res)
            if (res.length < 1) {
                 e.preventDefault();
                 $( "#rabNameDel :input" ).focus();
                 $( "#rabNameDel #berror" ).show();
             }
            });
           });

   |]
 
         
  return (res, wid)

deleteRabWid::AdoptRequestId->Widget
deleteRabWid  reqId= do
    (rabWid, r_enctype)<- handlerToWidget (generateFormPost (deleteRabbitForm  reqId))
    [whamlet|
     <form #delRab method=post action=@{DelRabR reqId} enctype=#{r_enctype}>
          ^{rabWid}
     |]
    toWidget [lucius|
               #delRab {
                 width:90%;
                 box-shadow:2px 2px 4px #7f7f7f;
                 background:#fbfbfb;
               }
         |]
      
getDelRabR::AdoptRequestId->Handler Html
getDelRabR reqId = adoptFormsPage (Just (deleteRabWid  reqId))

postDelRabR::AdoptRequestId->Handler Html
postDelRabR reqId = do
  ((result, _), _) <- runFormPost (deleteRabbitForm  reqId)
  case result of
    FormSuccess (RabAdopt rId rName date )-> do
      rab <- queryName rName
      if (length rab >0)
         then do
          let (Entity rabId arab) = head rab
          let adopt = Adopt rabId date rId 
          runDB $ delete $
            from $ \adopts -> do
            where_ (adopts ^. AdoptRab ==. val rabId)
          return ()
         else
          return ()
      redirect ViewAdoptForms

    _ -> defaultLayout $ [whamlet| Form Error |]


adoptRabbitsForm::AdoptRequestId->Html->MForm Handler (FormResult Day, Widget)
adoptRabbitsForm  arId extra = do
  local_time <- liftIO  getLocalTime
  let stime = showtime (localDay local_time)
  (dateRes, dateView)<-mreq textField "nope" (Just stime)
  let tdate = fmap (doparseTime.unpack) dateRes
  let res = tdate
  let wid = do
       $(widgetFileNoReload def "cancelButton")
       per<- handlerToWidget ( getPerson arId) 
       rabs<- handlerToWidget (getRabs arId)
       let rabNames = getNames rabs
       [whamlet| #{extra}
          <div #anRForm>
             $maybe person <- per
              <div #reqPer style="float:left; width:100%;">
               <div #perName style="float:left">
                Confirm adoption for  #{personFirstName person} #{personLastName person}
               <div .blDate #anDate style="float:left; padding-left:10%;">
                Date: ^{fvInput dateView}
               <div #dateError style="display:none; color:#ff0000; float:left">
                                     \<-Error
                                             
               <div .cancelBut #canBut, style="float:right; margin-left:20px;"> <a href=@{ViewAdoptForms}>cancel</a>
             <div #rabList sytle="width:100%; float:left;">
              <div #rabTitle style="float:left; width:100%;"> Rabbits:
              $forall Entity rId rabb <-rabs
                  <div #arab>
                    <div #rname style="padding-left:10px;"> #{rabbitName rabb}
             <input .subButton  type=submit value="confirm">
            |]
       toWidget [lucius|
                    #reqPer {
                     width:100%;
                    }
                   #anDate {
                      padding-right:10%;
                    }

                    #anRForm div {
                       float:left;
                     }
                    ##{fvId dateView} {
                        width:6em;
                        display:inline;
                      }
   |]

  return ( res, wid)

adoptRabWid::AdoptRequestId->Widget
adoptRabWid  reqId= do
    (rabWid, r_enctype)<- handlerToWidget (generateFormPost (adoptRabbitsForm  reqId))
    [whamlet|
     <form #adoptRab method=post action=@{AdoptRabR reqId} enctype=#{r_enctype}>
          ^{rabWid}
     |]
    toWidget [lucius|
               #adoptRab {
                 width:90%;
                 box-shadow:2px 2px 4px #7f7f7f;
                 background:#fbfbfb;
               }
         |]

getAdoptRabR::AdoptRequestId->Handler Html
getAdoptRabR reqId = adoptFormsPage (Just (adoptRabWid  reqId))

goAdopted::Day->Entity Adopt->AdoptedRabs
goAdopted aday (Entity selId (Adopt rab tdate req))= AdoptedRabs rab aday (Just req)

doDelete (Entity selId (Adopt rabId _ _ )) = delete $ from $ \sels -> do
  where_ (sels ^. AdoptRab ==. val rabId)

adoptedText::Textarea
adoptedText = Textarea ( pack "Adopted Rabbits")

postAdoptRabR::AdoptRequestId->Handler Html
postAdoptRabR reqId = do
  ((result, _), _) <- runFormPost (adoptRabbitsForm  reqId)
  case result of
    FormSuccess tdate -> do
      sels<- getSelectRecs reqId
      let adrabs = fmap (goAdopted tdate) sels
      runDB $ do
        insert (AdoptNotes reqId tdate adoptedText )
        mapM_  insert adrabs
      -- delete from adopt
        mapM doDelete sels
      -- update rabbit records
      

      redirect ViewAdoptForms
    _ -> do
         msgM<-getMessage
         defaultLayout $ [whamlet|
                           Message:
                          $maybe msg<- msgM
                           #{msg}
                          Form Error |]

editRabbitsForm::[Text]-> AdoptRequestId->Html->MForm Handler (FormResult RabAdopt, Widget)
editRabbitsForm bnames arId extra = do
  local_time <- liftIO  getLocalTime
  let stime = showtime (localDay local_time)
  (dateRes, dateView)<-mreq textField "nope" (Just stime)
  (rabbitRes,rabbitView)<-mreq textField (FieldSettings "nope" (Just "nope") (Just "rabName") (Just "rabName") []) Nothing
  let date = fmap (doparseTime.unpack) dateRes
  let res = RabAdopt arId <$> rabbitRes <*> date 
  let wid = do
       $(widgetFileNoReload def "cancelButton")
       per<- handlerToWidget ( getPerson arId) 
       rabs<- handlerToWidget (getRabs arId)
       [whamlet| #{extra}
          <div #anRForm>
             $maybe person <- per
              <div #reqPer>
               Select Rabbit for  #{personFirstName person} #{personLastName person}
               <div .cancelBut #canBut, style="float:right; margin-left:20px;"> <a href=@{ViewAdoptForms}>cancel</a>
             <div  #therab>
              <div .bllabel>
                Rabbit:
              <div #rabNameD>
               ^{fvInput rabbitView}
               <div #berror>
                \<- Not On File!

              <div .blDate #anDate style="float:right;">
                Date: ^{fvInput dateView}
              <div #dateError style="display:none; color:#ff0000; float:right">Error->
             <input .subButton  type=submit value="submit">
            |]
       toWidget [lucius|
                   #arabStatus {
                    padding-left:5%;
                   }
                   #therab {
                    width:100%;
                    margin-top:5px;
                   }
                   #reqPer {
                     width:100%;
                    }
                   #anDate {
                      padding-right:10%;
                    }

                    #anRForm div {
                       float:left;
                     }
                    #anRForm input {
                       display:inline-block;
                     }
                    ##{fvId rabbitView} {
                        width:20em;
                        float:left;
                      }

                    ##{fvId dateView} {
                        width:6em;
                      }
   |]
       toWidget [julius|
                  $( document ).ready(function() { 
                    $( "#rabName" ).attr("title", "Find rabbit by name");
                    $( "#rabName" ).autocomplete({
                      search:"",
                      source: #{rawJS (gostring bnames)},
                      select: function (event, ui) {
                          $( this ).val (ui.item.label);
                        }
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

                           

               $(function() { $( "#rabName" ).keydown( function( e) {
                           $( "#rabNameD #berror" ).hide();
                           if (e.keyCode==13 || e.keyCode==9) {
                            var aname = $( this ).val();
                            var nname = checkName( aname );
                            if (nname.length < 1) {
                              e.preventDefault();
                              $( this ).val( "" );
                              $( this ).focus();
                              $( "#rabNameD #berror" ).show();
                              } else {
                              $( this ).val( nname );
                              }

                            }
                   });
                 });

               $(function() { $( "#rabName" ).blur( function(e ) {
                            var aname = $( this ).val();
                            var nname = checkName( aname );
                            if (nname.length < 1) {
                              e.preventDefault();
                              $( this ).val( "" );
                              $( this ).focus();
                              $( "#rabNameD #berror" ).show();
                              } else {
                              $( this ).val( nname );
                              }

                            
                   });
                 });
              |]
  return ( res, wid)

adoptNotesForm::AdoptRequestId->Html->MForm Handler (FormResult AdoptNotes, Widget)
adoptNotesForm reqId extra= do
  local_time <- liftIO  getLocalTime
  let stime = showtime (localDay local_time)
  (dateRes, dateView)<-mreq textField "nope" (Just stime)
  (actionRes, actionView)<-mreq textareaField "nope" Nothing
  let date = fmap (doparseTime.unpack) dateRes
  let res = AdoptNotes reqId <$> date <*> actionRes
  let wid = do
       $(widgetFileNoReload def "cancelButton")
       per<- handlerToWidget ( getPerson reqId) 
       [whamlet| #{extra}
            <div #anForm>
             $maybe person <- per
              <div #reqPer>
               New action for #{personFirstName person} #{personLastName person}
               <div .cancelBut #canBut, style="float:right; margin-left:20px;">
                  <a href=@{ViewAdoptForms}>cancel</a>
               <div .blDate #anDate style="float:right; margin-right:5%;">
                 Date: ^{fvInput dateView}
               <div #dateError style="display:none; color:#ff0000; float:right;">Error->
             <div .required #action>
              <div .bllabel>
               Action:
              ^{fvInput actionView}
             <input .subButton  type=submit value="submit">
            
            |]
       toWidget [lucius|
               ##{fvId actionView} {
                     width:95%;
                 }
               ##{fvId dateView} {
                    width:6em;
                    padding-right:1%;
                   }

               #anForm {
                   width:100%;
                 }
               #reqPer {
                 width:100%;
               }
               #anDate input {
                   display:inline;
                  }
              
               #arabStatus {
                 padding-left:20px;
                }

               #anDate {
                 float:right;
                }
               #reqPer {
                 float:left;
                }
               #action {
                 float:left;
                 width:100%;
                }
          |]
  
  return (res, wid)

rabbitsWidget::AdoptRequestId->Widget
rabbitsWidget reqId = do
  rabs <- handlerToWidget  (getRabs reqId)
  let isRabs = (length rabs)>0
  [whamlet|
    <div #RabbitBlock style="margin-left:2px; padding-left:2px;">
      <div .bllabel style="padding-top:5px; padding-left:10px;">
         Rabbits: #
      $if isRabs    
       $forall Entity rabId rab <-rabs
        <div #arabname>
         <a href=@{ViewR rabId}>#{rabbitName rab}</a>
      $else
        <div #none style="padding-top:5px;">
         None Selected
      |]

adoptedRabbitsWidget:: AdoptRequestId->Widget
adoptedRabbitsWidget reqId = do
  
  rabs <- handlerToWidget  (getAdoptedRabs (Just reqId))
 
  let isaRabs = (length rabs)>0
  [whamlet|
    <div #RabbitBlock style="margin-left:2px; padding-left:2px;">
      <div .bllabel style="padding-top:5px; padding-left:10px;">
         Adopted Rabbits: #
      $if isaRabs    
       $forall Entity rabId rab <-rabs
        <div #adrabname>
         <a href=@{ViewR rabId}>#{rabbitName rab}</a>
      |]
    
notesWidget::AdoptRequestId->Widget
notesWidget aid = do
  notes<- handlerToWidget $ (getNotes aid) 
  [whamlet|
   <div #adoptNoteBlock style="width:100%;">
    $forall (Entity nid (AdoptNotes reqId date action))<-notes
      <div .afrow #anote style="padding-left:8px;">
       <div #arDate>
        #{showtime date} :
       <div #arAction style="padding-left:5px;">
        #{action}
    |]

selRabWid::AdoptRequestId->Widget
selRabWid  reqId= do
    bnames <- handlerToWidget getAdoptAvailable
    (rabWid, r_enctype)<- handlerToWidget (generateFormPost (editRabbitsForm bnames reqId))
    [whamlet|
     <form #selRab method=post action=@{SelRabR reqId} enctype=#{r_enctype}>
          ^{rabWid}
     |]
    toWidget [lucius|
               #selRab {
                 width:90%;
                 box-shadow:2px 2px 4px #7f7f7f;
                 background:#fbfbfb;
               }
         |]
      
getSelRabR::AdoptRequestId->Handler Html
getSelRabR reqId = adoptFormsPage (Just (selRabWid  reqId))

postSelRabR::AdoptRequestId->Handler Html
postSelRabR reqId = do
  ((result, _), _) <- runFormPost (editRabbitsForm []  reqId)
  case result of
    FormSuccess (RabAdopt rId rName date )-> do
      rab <- queryName rName
      if (length rab >0)
         then do
          let (Entity rabId arab) = head rab
          let adopt = Adopt rabId date rId 
          runDB $ insert adopt
          return ()
         else
          return ()
      redirect ViewAdoptForms

    _ -> defaultLayout $ [whamlet| Form Error |]

  
getNewNoteWid reqId= do
  (tnotesWidget, nenc)<- handlerToWidget $ generateFormPost (adoptNotesForm reqId)
  [whamlet|
     <form #getNote method=post action=@{NewNoteR reqId} enctype=#{nenc}>
          ^{tnotesWidget}
     |]
  toWidget [lucius|
            #getNote {
              width:90%;
              box-shadow:2px 2px 4px #7f7f7f;
              background:#fbfbfb;
            }
   |]

getNewNoteR::AdoptRequestId->Handler Html
getNewNoteR reqId = adoptFormsPage (Just (getNewNoteWid reqId))

postNewNoteR::AdoptRequestId->Handler Html
postNewNoteR reqId = do
  ((result,_),_) <- runFormPost (adoptNotesForm reqId)
  case result of
    FormSuccess note-> do
      runDB $ do
        insert note
        return()
      redirect ViewAdoptForms
    _ -> redirect ViewAdoptForms


adoptFormsPage::Maybe Widget->Handler Html
adoptFormsPage aform = do
  (formWidget, enctype)<- generateFormPost getNameForm
  adopts<-getAdoptions
  bnames <-  getNamesDB
  maid <- maybeAuthId
  auth <- isAdmin
  impath <- liftIO getImagePath
  let imgpath = unpack impath
  let mode = (maid == Just "demo")
  let isAuth=(auth==Authorized)
  impath <- liftIO getImagePath
  let imgpath = unpack impath

  admin <- isAdmin
          
  defaultLayout $ do
    setTitle "Adoption Requests"
    $(widgetFileNoReload def "cancelbutton")
    $(widgetFileNoReload def "bldate")
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
    addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
    addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"
    
    [whamlet|
     <div #blHeaderD>
     ^{getNameWidget bnames formWidget enctype}
     ^{headerLogWid imgpath maid}    
     ^{mainMenu mode}
     <div #afTitle style="width:100%; float:left; text-align:center; background:#cfcfcf;"> 
           <b> Adoption Requests </b>
    $maybe awid<-aform
        ^{awid}
    <div #adoptReqBlock>
     $forall (Entity aid (AdoptRequest date apid adoptinfo afile), Entity pId per)<-adopts
        <div .afrow>
         $maybe _ <- adoptinfo 
          <a href=@{ViewAdoptForm aid}>#{personLastName per}, #{personFirstName per} :&nbsp;
                       #{showtime (date)} 
         $maybe file <- afile 
          <a href=#{mkLink file imgpath}>#{personLastName per}, #{personFirstName per} :&nbsp;
                       #{showtime (date)}
         <div #adbuttons>
          <div .aButton #addNote><a href=@{NewNoteR aid}>New Action</a>
          <div .aButton #addRab><a href=@{SelRabR aid}>Select Rabbit</a>
          <div .aButton #delRab><a href=@{DelRabR aid}>Remove Rabbit</a>
          <div .aButton #adopt><a href=@{AdoptRabR aid}>Adopt</a>
        <div .afrow>
         ^{rabbitsWidget aid}
         ^{adoptedRabbitsWidget aid}
        ^{notesWidget aid}
 
      |]
    toWidget [lucius|

#adoptReqBlock   #adbuttons div {
       float:right;
   }

#adoptReqBlock   #adbuttons {
       float:right;
   }
   .afrow {
    width:100%;
    margin-bottom:2px;
    margin-top:2px;
   border-bottom:1px dotted #7f7f7f;
   }

   .afrow a {
     float:left;
     margin:4px;
  }
     

 #adoptReqBlock div {
    float:left;
  }

  .aButton {
     float:right;   
  }

 #adoptReqBlock .aButton {
      float:right;
 }

 .aButton a {
    border:1px solid #7f7f7f;
    border-radius:5px;
    padding-left:4px;
    padding-right:4px;
    padding-top:1px;
    padding-bottom:1px;
    font-size:90%;
    text-decoration:none;
    background:#efefef;
    color:#000000;
}

 .aButton a:hover {
  border:1px solid #ff7f7f;
  background:#ffffff;
}

  #adoptReqBlock #addNote {
    float:right;
   }
     
 |]

getViewAdoptForms::Handler Html
getViewAdoptForms  = adoptFormsPage Nothing


viewaform::Person->AdoptInfo->Widget
viewaform person adoptinfo= $(widgetFileNoReload def "viewAdoptForm");

doBool::Bool->Text
doBool val = if val then "Yes" else "No"

getViewAdoptForm::AdoptRequestId->Handler Html
getViewAdoptForm  id = do
  (formWidget, enctype)<- generateFormPost getNameForm
  aform<- getAdoptForm id
  bnames <-  getNamesDB
  maid <- maybeAuthId
  impath <- liftIO getImagePath
  let imgpath = unpack impath

  admin <- isAdmin
  defaultLayout $ do
    setTitle "Adoption Request"
    $(widgetFileNoReload def "cancelbutton")
--         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
--         addScriptRemote "//code.jquery.com/ui/1.11.0/jquery-ui.js"
--         addStylesheetRemote "//code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css"     
    [whamlet|
     <div #blHeaderD>
     ^{getNameWidget bnames formWidget enctype}
     ^{headerLogWid imgpath maid}    
    <div #afTitle style="width:100%; float:left; text-align:center; background:#cfcfcf;"> 
           <b> Adoption Request </b>
    <div #adoptReqBlock style="float:left;">
     $forall (Entity  aide  (AdoptRequest date apid adoptinfo file) , Entity pId per)<- aform
          <div #afDate style="float:right; width:100%; text-align:right;">
            #{showtime (date)}
          $maybe ainfo <- adoptinfo
           ^{viewaform per ainfo}
          $maybe afile <- file
            <a href=#{mkLink afile imgpath}>#{afile}</a>
    |]
          

