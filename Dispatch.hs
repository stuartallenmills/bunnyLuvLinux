{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dispatch where

import Yesod
import Yesod.Auth

import Foundation
import Handler.Home
import Handler.Add
import Handler.VetVisit
import Handler.Adopted
import Handler.Wellness
import Handler.Image
import Handler.Reports
import Handler.Admin
import Handler.Search
import Handler.Bonded
import Handler.Treatment
import Handler.ViewRabbit
import Handler.DailyReport
import Handler.AdoptionForm
import Handler.ViewAdoption
import Handler.AdoptionReq
import Handler.Vets

mkYesodDispatch "App" resourcesApp
