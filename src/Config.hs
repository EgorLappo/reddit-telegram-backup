module Config 
  ( Config(..)
  , RedditConfig(..)
  , SQLConfig(..)
  , TelegramConfig(..)
  , defaultConfig
  ) where 

import GHC.Generics ( Generic )
import Data.Text ( Text )


newtype SQLConfig = SQLConfig 
  { databaseFile :: Text 
  } deriving (Show, Generic)

data TelegramConfig = TelegramConfig
  { apiToken :: Text 
  , tgApiBaseUrl :: Text
  , chatId :: Text
  } deriving (Show, Generic)


data RedditConfig = RedditConfig
  { clientID :: Text
  , clientSecret :: Text
  , username :: Text
  , password :: Text
  , userAgent :: Text
  , redditAuthBaseUrl :: Text
  , redditApiBaseUrl :: Text
  , postLimit :: Int
  } deriving (Show, Generic)

data Config = Config {
    redditConfig :: RedditConfig,
    databaseConfig :: SQLConfig, 
    telegramConfig :: TelegramConfig
} deriving (Show, Generic)


redditCfg :: RedditConfig
redditCfg = RedditConfig {
  clientID = "YOUR_REDDIT_CLIENT_ID",
  clientSecret = "YOUR_REDDIT_CLIENT_SECRET",
  username = "YOUR_REDDIT_USERNAME",
  password = "YOUR_REDDIT_PASSPORT",
  userAgent = "telegram-backup by Egor Lappo",
  redditAuthBaseUrl = "https://www.reddit.com/api/v1/access_token",
  redditApiBaseUrl = "https://oauth.reddit.com",
  postLimit = 100
}

sqlCfg :: SQLConfig
sqlCfg = SQLConfig {
  databaseFile = "sqlite.db"
}

tgCfg :: TelegramConfig 
tgCfg = TelegramConfig 
  { apiToken = "YOUR_TELEGRAM_BOT_TOKEN:AAHEq9OW9nI6hACNHqE1_Pb88A-4V9uSjNo"
  , tgApiBaseUrl = "https://api.telegram.org"
  , chatId = "YOUR_TELEGRAM_CHAT_ID" -- append -100 to the chat id
  }

defaultConfig :: Config
defaultConfig = Config 
  { redditConfig = redditCfg
  , databaseConfig = sqlCfg
  , telegramConfig = tgCfg
  }