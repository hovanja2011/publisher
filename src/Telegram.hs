{-# LANGUAGE OverloadedStrings #-}

module Telegram where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS

sendMessage :: String -> String -> String -> IO ()
sendMessage token chatId text = do
    let settings =
            managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

    manager <- newManager settings

    request <- parseRequest $
        "POST https://api.telegram.org/bot"
        ++ token
        ++ "/sendMessage"

    let requestWithBody =
            urlEncodedBody
                [ ("chat_id", BS.pack chatId)
                , ("text", BS.pack text)
                ]
                request

    response <- httpLbs requestWithBody manager

    putStrLn $ "Telegram response status: "
        ++ show (responseStatus response)
