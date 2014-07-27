{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module Handler.Image where

--this is a test 

import qualified Data.ByteString.Lazy as L
import Conduit

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod hiding ((!=.), (==.), update, (=.))
import Yesod.Default.Util
import Foundation
import Yesod.Auth
import Data.Text (Text, unpack, pack)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)
import Text.Printf
import Control.Applicative
import Data.Time.LocalTime
import Data.Time
import System.FilePath
import FormUtils




headerWidget::Widget
headerWidget = $(widgetFileNoReload def "header")

    

uploadForm :: Html -> MForm Handler  (FormResult (FileInfo), Widget)
uploadForm  = renderDivs $ fileAFormReq "Image file"
  

getImagesR::RabbitId-> Handler Html
getImagesR rabId = do
  ((_, widget), enctype) <-runFormPost uploadForm
  Just rabbit <-runSqlite "test5.db3"  $ do get rabId
  let menu = [whamlet| <b> Upload Rabbit Image for #{rabbitName rabbit}
                <div #wellCan style="float:right; display:inline;">
                  <div .cancelBut #wellEdCan style="display:inline; float:right;">
                   <a href=@{ViewR rabId }> cancel </a>
                     |]
  let form = [whamlet|            
                <form method=post enctype=#{enctype}>
                  ^{widget}
                 <input .btn type=submit value="Upload">
      |]
  baseForm "Upload Image" menu form

postImagesR ::RabbitId-> Handler Html
postImagesR rabId = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
        FormSuccess (file) -> do
            -- TODO: check if image already exists
            -- save to image directory
            filename <- writeToServer file
--            _ <- runDB $ insert (Image filename info date)
            runSqlite "test5.db3" $ do
              update $ \p -> do
               set p [RabbitImage =. val (Just (pack filename))]
               where_ (p ^. RabbitId ==. val rabId)
               return ()
            setMessage "Image saved"
            redirect (ViewR rabId)
        _ -> do
            setMessage "Something went wrong"
            redirect HomeR

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    today<- liftIO  getCurrentDay
    uploadDir <- liftIO getUploadDir
    let date = showfiletime today
    let filename = unpack $ fileName file
        rf = reverse filename
        (ext, thead) = break (== '.') rf
        thead2 = tail thead
        fn = (reverse thead2) ++ "_"++( date) ++ "." ++ (reverse ext)
        path = imageFilePath uploadDir fn
    liftIO $ fileMove file path
    return fn

imageFilePath :: Text->String -> FilePath
imageFilePath adir f = (unpack adir) </> f
