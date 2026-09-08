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


main :: IO ()
main = do
    user <- getEnv "MAIL_USER"
    password <- getEnv "MAIL_PASSWORD"

    conn <- connectIMAPSSL "imap.mail.ru"
    putStrLn "Connected."

    login conn user password
    putStrLn "Logged in."

    select conn "INBOX"
    putStrLn "INBOX selected."

    statusInfo <- status conn "INBOX" [UIDNEXT]

    case statusInfo of
        [(UIDNEXT, nextUid)] -> do
            let latestUid = fromIntegral (nextUid - 1)

            email <- fetchPeek conn latestUid

            printEmail email

        _ ->
            putStrLn "Could not get UIDNEXT."

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