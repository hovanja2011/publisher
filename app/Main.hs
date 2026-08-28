module Main where

import Config (getBotToken, getChatId)
import Telegram (sendMessage)

main :: IO ()
main = do
    botToken <- getBotToken
    chatId <- getChatId

    case (botToken, chatId) of
        (Just token, Just chat) ->
            sendMessage token chat "Hello from Haskell publisher!"

        _ -> do
            putStrLn "Required environment variables are missing."