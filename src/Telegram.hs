{-# LANGUAGE OverloadedStrings #-}

module Telegram where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

sendMessage :: String -> String -> String -> IO ()
sendMessage token chatId text = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest $
        "POST https://api.telegram.org/bot"
        ++ token
        ++ "/sendMessage"

    let requestWithBody =
            urlEncodedBody
                [ ("chat_id", BS.pack chatId)
                , ("text", TE.encodeUtf8 (T.pack text))
                ]
                request

    response <- httpLbs requestWithBody manager

    putStrLn $
        "Telegram response status: "
            ++ show (responseStatus response)

    let body = responseBody response

    case Aeson.decode body :: Maybe Aeson.Value of
        Just (Aeson.Object obj) ->
            case Aeson.parseMaybe (Aeson..: "ok") obj of
                Just True ->
                    putStrLn "Telegram: message sent successfully."

                Just False ->
                    fail "Telegram API returned ok=false."

                Nothing ->
                    fail "Telegram response does not contain 'ok'."

        _ ->
            fail "Could not parse Telegram response as JSON."