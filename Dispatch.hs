{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dispatch where

import Yesod

import Foundation
import Handler.Home
import Handler.Preview
import Handler.Add
import Handler.VetVisit
import Handler.Adopted
import Handler.Wellness
import Handler.Image

mkYesodDispatch "App" resourcesApp
