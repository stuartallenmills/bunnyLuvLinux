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


mkYesodDispatch "App" resourcesApp
