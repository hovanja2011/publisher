{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL
import System.Environment (getEnv)

import qualified Data.ByteString as B
import qualified Data.Text.IO as T

import Data.MIME (defaultCharsets, HasHeaders, WireEntity, transferDecoded', contentType, matchContentType, mime, entities)
import Data.IMF (headerSubject, parse, message)

import Control.Lens
import Data.MIME.Charset (charsetText')
import qualified Data.Text as T

import Telegram (sendMessage)

main :: IO ()
main = do
    user <- getEnv "MAIL_USER"
    password <- getEnv "MAIL_PASSWORD"
    sender <- getEnv "MAIL_SENDER"

    telegramToken <- getEnv "TELEGRAM_BOT_TOKEN"
    telegramChatId <- getEnv "TELEGRAM_CHAT_ID"

    conn <- connectIMAPSSL "imap.mail.ru"
    putStrLn "Connected."

    login conn user password
    putStrLn "Logged in."

    select conn "INBOX"
    putStrLn "INBOX selected."

    uids <- search conn [FROMs sender, NEWs]

    case uids of
        [] -> do
            putStrLn "No new publication email."

        _ -> do
            let uid = last uids

            putStrLn "New publication email found."

            email <- fetchPeek conn uid

            result <- getEmailContent email

            case result of
                Nothing ->
                    putStrLn "Could not parse email."

                Just (subject, body) -> do
                    putStrLn "--- SUBJECT ---"
                    putStrLn subject

                    putStrLn "--- BODY ---"
                    putStrLn body

                    let telegramText =
                            subject ++ "\n\n" ++ body

                    sendMessage
                        telegramToken
                        telegramChatId
                        telegramText

                    store conn uid (PlusFlags [Seen])

                    putStrLn "Email marked as processed."

    logout conn

printEmail :: B.ByteString -> IO ()
printEmail email =
    case parse (message mime) email of
        Left err ->
            putStrLn ("Parse error: " ++ err)

        Right msg -> do
            putStrLn "--- SUBJECT ---"

            case view (headerSubject defaultCharsets) msg of
                Just subject ->
                    T.putStrLn subject

                Nothing ->
                    putStrLn "Subject not found."

            putStrLn "\n--- BODY ---"

            case firstOf
                    (entities . filtered isTextPlain)
                    msg of

                Just entity ->
                    printTextBody entity

                Nothing ->
                    putStrLn "text/plain body not found."

isTextPlain :: HasHeaders a => a -> Bool
isTextPlain =
    matchContentType "text" (Just "plain")
        . view contentType

printTextBody :: WireEntity -> IO ()
printTextBody entity =
    case preview
            (transferDecoded'
                . _Right
                . charsetText' defaultCharsets
                . _Right)
            entity of

        Just text ->
            T.putStrLn text

        Nothing ->
            putStrLn "Could not decode text/plain body."

getEmailContent :: B.ByteString -> IO (Maybe (String, String))
getEmailContent email =
    case parse (message mime) email of
        Left err -> do
            putStrLn ("Parse error: " ++ err)
            return Nothing

        Right msg -> do
            let subject =
                    case view (headerSubject defaultCharsets) msg of
                        Just s  -> T.unpack s
                        Nothing -> ""

            let body =
                    case firstOf
                            (entities . filtered isTextPlain)
                            msg of

                        Just entity ->
                            case preview
                                    (transferDecoded'
                                        . _Right
                                        . charsetText' defaultCharsets
                                        . _Right)
                                    entity of
                                Just text -> T.unpack text
                                Nothing   -> ""

                        Nothing ->
                            ""

            return (Just (subject, body))