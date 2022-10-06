module TelegramServant (TelegramBotAPI) where

import Data.Text (Text)
import Servant.API

import Data.Aeson

type TelegramBotAPI = Capture "token" Text 
                   :> "sendMessage" 
                   :> QueryParam "chat_id" Text 
                   :> QueryParam "text" Text 
                   :> QueryParam "parse_mode" Text
                   :> Post '[JSON] Value 