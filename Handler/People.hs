{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.People where


import Conduit
import Data.Default
import Yesod hiding ((!=.), (==.), (=.), update)
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Calendar
import Text.Julius
import FormUtils
import Utils

personFormSolo::Maybe Person -> Html -> MForm Handler (FormResult Person, Widget)
personFormSolo pm  extra = do
   (res, personWidget) <- (personForm pm)
   let wid = [whamlet| #{extra}
                       ^{personWidget} |]
   return (res, wid)

getPerson::PersonId->Handler [Entity Person]
getPerson pId = runDB $
      select $ from $ \p-> do
      where_ (p ^. PersonId ==. val pId)
      return p

getPersonP pid = runDB $ get pid

queryPeople = runDB $
      select $ from $ \p -> do
      where_ (p ^. PersonLastName !=. val "")
      return p

getViewPeopleR::Handler Html
getViewPeopleR = do
  people<- queryPeople
  let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                 <b> View People on file.  Select to View/Edit Details</b>
                <div #vvCan style="float:right; display:inline;">
                  <div .cancelBut #vvEdCan style="display:inline; float:right;">
                   <a href=@{HomeR}> cancel </a>
               |]           
  let form= [whamlet|
     <div #people>
       $forall (Entity pid person) <- people
         <div #person>
          <a href=@{EditPersonR pid}> #{personFirstName person} #{personLastName person} #{personPhone person}</a>
     |]
  baseForm "View People" menu form

getEditPersonR::PersonId->Handler Html
getEditPersonR pid = do
  person<-getPerson pid
  let (Entity pid aper)= head person
  (formWidget, enctype)<- generateFormPost (personFormSolo (Just  aper))
  let menu = [whamlet|
              <div #addCance style="float:inherit; text-align:left; margin:10px;">
                 <b> Edit Person </b>
                <div #vvCan style="float:right; display:inline;">
                  <div .cancelBut #vvEdCan style="display:inline; float:right;">
                   <a href=@{ViewPeopleR}> cancel </a>
               |]
  let form = [whamlet|
    <form method=post action=@{EditPersonR pid} enctype=#{enctype}>
      <div #epH>
      ^{formWidget}
      <input type=submit value="submit">
      |]
  baseForm "Edit Person" menu form

postEditPersonR::PersonId->Handler Html
postEditPersonR pid = do
  personB<-getPerson pid
  let (Entity _ aper) = head personB
  ((result, _), _) <- runFormPost (personFormSolo (Just  aper))
  case result of
    FormSuccess aperson-> runDB $ do
                         _ <- replace pid aperson
                         return ();
    _ ->   return ()
  redirect (EditPersonR pid)

