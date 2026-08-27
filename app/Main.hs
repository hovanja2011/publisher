module Main where

import Config (getBotToken, getChatId)

main :: IO ()
main = do
    botToken <- getBotToken
    chatId <- getChatId

    putStrLn $ case botToken of
        Just _  -> "TELEGRAM_BOT_TOKEN: found"
        Nothing -> "TELEGRAM_BOT_TOKEN: missing"

    putStrLn $ case chatId of
        Just _  -> "TELEGRAM_CHAT_ID: found"
        Nothing -> "TELEGRAM_CHAT_ID: missing"