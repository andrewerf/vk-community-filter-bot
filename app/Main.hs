{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import System.IO

import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString        as BS

import qualified Data.Aeson             as Aeson

import           Network.Wai
import           Network.HTTP.Types       (status200)
import           Network.Wai.Handler.Warp (run)

import           VkApi


respondSuccess text respond = respond $ responseLBS status200 [("Content-Type", "text/plain")] text
respondOk = respondSuccess "ok"


processCallback :: (Maybe CallbackEvent) -> (Response -> IO ResponseReceived) -> IO ResponseReceived
processCallback (Just CallbackConfirmationEvent) respond = do
    putStrLn "Enter confirmation code: "
    code <- BS.getLine
    respondSuccess (BSL.fromStrict code) respond

processCallback (Just (CallbackWallReplyEvent event)) respond = do
    putStrLn (show event)
    respondOk respond

processCallback Nothing respond = do
    putStrLn "Got Nothing"
    respondOk respond

processCallback (Just CallbackUnknownEvent) respond = do
    putStrLn "Got Unknown event"
    respondOk respond


app :: Application
app request respond = do
    body <- lazyRequestBody request

    let callback = (Aeson.decode body) :: Maybe CallbackEvent
    processCallback callback respond

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Hi"
    run 80 app