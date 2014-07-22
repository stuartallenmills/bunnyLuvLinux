{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Auth where

import           Data.Default         (def)
import           Data.Text            (Text, unpack, pack)
import           Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Message
--import           Yesod.Auth.Dummy -- just for testing, don't use in real life!!!

authBunnyluv :: YesodAuth m => AuthPlugin m
authBunnyluv =
    AuthPlugin "bunnyluv" dispatch login
  where
    dispatch "POST" [] = do
        ident <- lift $ runInputPost $ ireq textField "ident"
        pass <-lift $ runInputPost $ ireq passwordField "pass"
        if (pass == "bunnyluv") then
         lift $ setCredsRedirect $ Creds "bunnyluv" ident []
        else loginErrorMessageI  LoginR PassMismatch
{-
         where
          errorMessage::Text
          errorMessage = " Password Incorrect"

          chkpasswordField = check testPass passwordField

          testPass pass
                | pass =="bunnyluv" = Right pass
                | otherwise = Left errorMessage
-}

    dispatch _ _ = notFound
    url = PluginR "bunnyluv" []
    login authToMaster =
        toWidget [hamlet|
$newline never
<form method="post" action="@{authToMaster url}">
    <b> BunnyLuv Login <br>
    <p>Username: #
     <input type="text" name="ident">

    <p>Password: #
     <input type="password" name="pass">
    <p>
    <input type="submit" value="Login">

|]



