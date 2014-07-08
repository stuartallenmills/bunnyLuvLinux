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

mkYesodDispatch "App" resourcesApp
