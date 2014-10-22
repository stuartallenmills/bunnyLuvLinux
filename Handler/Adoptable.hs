{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Adoptable where

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
import FormUtils


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
     adoptablePage Nothing

postAdoptableR = do
  ((result,_),_)<- runFormPost (getAgeForm Nothing)
  case result of
    FormSuccess adsearch->
          adoptablePage (Just adsearch)
    _ -> redirect AdoptableR

adoptSearchWid  wid enctype = do
      [whamlet|
        <form #adoptSearchForm method=post action=@{AdoptableR} enctype=#{enctype}>
            ^{wid}
          |]
      toWidget [lucius|

        #adoptSearchForm #agesub {
          float:right;
        }

        #getAgeDiv div {
          margin-bottom:0px;
          margin-top:0px;
          height:25px;
        }

        #adoptSearchForm {
         background:#fafafa;
         font-size:95%;
         padding:0.4em;
        }

        input[type="checkbox"] {
          transform:translateY(2px);
        }

        #getAgeDiv {
           width:90%;
        }
        #ageInD label {
           width:20%;
        }
        #sex {
          margin-left:20px;
         }
        #ageTitle {
          display:none;
        }
        #adoptSearchForm input {
                display:inline;
                margin-left:5px;
         }
        #adoptSearchForm div {
           float:left;
         }

        #companion {
           margin-left:20px;
         }

        #agesub {
          float:right;
         }

                |]

getYears Nothing = 0
getYears (Just (AgeSearch Nothing _ _ _ _ _)) = 0
getYears (Just (AgeSearch (Just as) _ _ _ _ _)) = as

getMonths Nothing = 0
getMonths (Just as) = agesearchDiff as

getBool Nothing _ =  True
getBool (Just as) f = tval where
                 (Just tval) = f as

widIntro::Widget
widIntro = do
  [whamlet|
      <div #intro>
        Find  Buns to Love from BunnyLuv!
    <div #body>
     We have over 100 wonderful rabbits eager for a new home.  You can use the search bar to narrow your search by age, sex, or rabbits with friends or families!  You can apply for adoption <a href=@{AdoptionFormR}>online</a> or <a href="http://media.wix.com/ugd/1f3057_f99ed5e1bbcbb4910ac62c58febb82b1.doc?dn=%22BLRRC_adoption_app.doc%22">download</a> the form as a Word document that you can email or mail to us.   To review our Adoption Policies click <a href="#policies">here</a>
 <div>
   By adopting a rabbit from BunnyLuv, you are giving the gift of life and creating space for us to save more!​
​​​ <div> If that's not enough reason to adopt from BunnyLuv Rabbit Resource Center, consider these extra benefits:

  <ul>
   <li>  Our rescued rabbits are have been spayed/neutered, healthy, socialized and litter-box trained. They are ready for your home.
   <li> With over 1700 successful "Luv Connections" to date, we specialize in facilitating the often difficult bonding process for your rabbits.
   <li> We offer help and guidance throughout the transition of your new rabbit group at home, and we are available afterwards,  should you need our help in any area of rabbit care.                                                                                                                                                                                       
  |]
  toWidget [lucius|
              #intro {
              text-align:center;
              font-size:250%;
              font-weight:150%;
              float:left;
              width:100%;
              background:#fefefe;
              padding-bottom:10px;
              font-style:italic;
              font-family:Comic Sans MS;
             }
            #body {
              padding-left:20px;
              padding-right:20px;
              margin-bottom:10px;
            }
            #policies {
               width:100%
               margin:5px;
               float:left;
              }
            #policies div {
               margin:5px;
               float:left;
            }

   |]      

--blockWid::Widget
blockWid imgpath today rId rab rabstoryM = [whamlet|
       <div .rabBlock >
           <a .rabTarget ##{rabbitName rab}>
            $maybe img <- rabbitImage rab
             <div #imgBlock style="background-image:url('#{mkLink img imgpath}');">
            $nothing 
             <div #imgBlock style="background-image:url('#{mkLink "bunnyluvWide.jpg" imgpath}');">

           <div #story>
             <div #nameLine style="width:100%; border-bottom:1px solid #8f8f8f;">
              <div #rName>
                <b> #{rabbitName rab}
              <div #rSex>
                (#{rabbitSex rab})
              <div #rAcq>
               Acq: #{showtime (rabbitDateIn rab)}
              <div #rAge>
                 Est. Age: #{getCurrentYears today rab}y #{getCurrentMonths today rab}m
             
             $maybe (Entity sId (RabbitStory rId rstory spneed adrule))<- rabstoryM
               <div #stry>
                  #{rstory}
               $maybe spn <- spneed
                  <div #spneed>
                   NEEDS: #{spn}
               $nothing
             $nothing
              <div #nothing>
                   #{rabbitName rab} would like a good home.
             <div #ff style="width:100%; border-top:1px solid #6f6f6f;">
             ^{ffWid rId}
             |]
            
aWid::Widget
aWid =  $(widgetFileNoReload def "Adoptable")
policyWid::Widget
policyWid = [whamlet|
     <div #thepolicies style="width:100%; float:left;">
       <div id="policies" sytle="width:100%; float:left;">
           <div>  <h2> Adoption Policies </h2>
           <div>  <b>Primary Caregiver. </b> When a rabbit is adopted from BunnyLuv Rabbit Resource Center, 
                 the primary caregiver must be a responsible adult.  The rabbit should be treated as an integral part of the family.  
              We do NOT adopt out rabbits as pets for children or for classrooms.
           <div>  <b>Indoor Housing. </b> Adopters of BunnyLuv Rabbit Resource Center rabbits 
              must understand that our rabbits are to live as household companions.  
             This means that they must have their living space indoors, and must spend every night indoors.  
             Additional safety precautions may be required and will be determined at the time of the home visit.
           <div> <b>Social Requirements. </b> It is our strong belief rabbits need other rabbits 
              in their daily lives, in addition to their humans.  
              For this reason, BunnyLuv does not adopt rabbits to live as single animals.  
              We specialize in rabbit introductions and will help you to expand your family.
           <div>
            <b>Returns. </b> If there are significant problems with the adopted rabbit, 
                the adopter needs to give us advance notice.  All rabbits adopted from this agency
                 must be returned to this agency in case of insurmountable problems that prevent the adoption from being permanent.
           <div>  <b>Adoption fees. </b> Our adoption fee is a $95 donation per rabbit.  
             Adoption fees are donations that cannot be refunded.
             We are a federally recognized tax-exempt, non-profit organization, 
            and donations made to us are no more refundable than they are to any other public charity.
           <div> 
             If you would like to hear more about adopting a second or third (or fourth or fifth...)
               rabbit from BunnyLuv, please call us at (818) 988 4488 or email <a href="mailto:bunnyluv@bunnyluv.org?Subject=Adoption%20Info">bunnyluv@bunnyluv.org</a> 


             |]
--aHWid::Widget
aHWid formWidget enctype today imgpath result ffresult=      [whamlet|
       ^{aWid}
       ^{adoptSearchWid formWidget enctype}
    <div #thePage>
        $if (not (null result))
         <div #Header>
          <div #headtext>
           Single Rabbits
          <div #blurb>
              Our single Rabbits are very friendly and we will help you integrate them into your family.
       $forall (Entity rId rab, rabstoryM) <-result
        ^{blockWid imgpath today rId rab rabstoryM}
      $if (not (null ffresult))
       <div #Header>
          <div #headtext>
            Friends and Family
          <div #blurb>
              We like to keep our rabbits with friends or families together.  These rabbits are great if you are getting your first rabbits, or if you want to add more than one new rabbit to your rabbit family!
       $forall (Entity rId rab, rabstoryM) <-ffresult
          ^{blockWid imgpath today rId rab rabstoryM}
      ^{policyWid}
 
        |]
                                                     
getResult today  adoptSearch avail = result where
             yrs= getYears adoptSearch
             result = if (yrs==0) then avail else newresult where
                diffMnths = getMonths adoptSearch
                bday = addDays (yrs*(-365)) today
                mnthDays = 31*diffMnths
                newresult = sortEnt mnthDays bday avail
                
adoptablePage adoptSearch = do
     (formWidget, enctype)<- generateFormPost (getAgeForm adoptSearch)
     avail<-queryCompanion (getBool adoptSearch male) (getBool adoptSearch female)
                      False (getBool adoptSearch noff)
     let hff = getBool adoptSearch hasff
     ff <- queryCompanion (getBool adoptSearch male) (getBool adoptSearch female)
                      (getBool adoptSearch hasff) False
     today <- liftIO getCurrentDay
     let noff = getResult today adoptSearch avail
     let ffres = getResult today adoptSearch ff
  
     impath <- liftIO getImagePath
     let imgpath = unpack impath
     msg <-getMessage
     maid <- maybeAuthId
     auth <- isAdmin
     let mode =  (maid == Just "demo")
     baseAdoption "Adoptable Rabbits" widIntro (aHWid formWidget enctype today imgpath noff ff)
 --    defaultLayout $ [whamlet| ^{aHWid formWidget enctype today imgpath result} |]

 
