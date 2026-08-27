module Config where

import System.Environment (lookupEnv)

getBotToken :: IO (Maybe String)
getBotToken = lookupEnv "TELEGRAM_BOT_TOKEN"

getChatId :: IO (Maybe String)
getChatId = lookupEnv "TELEGRAM_CHAT_ID"