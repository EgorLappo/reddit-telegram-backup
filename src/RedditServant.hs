module RedditServant (RedditAuthAPI, RedditSavedPostsAPI) where 

import Data.Text (Text)
import Servant.API


import RedditTypes

-- AUTHENTICATION 

-- Servant setup 
type RedditAuthAPI = BasicAuth "reddit" User  
                 :> QueryParam "grant_type" Text 
                 :> QueryParam "username" Text 
                 :> QueryParam "password" Text 
                 :> Header "User-Agent" Text 
                 :> Post '[JSON] AccessToken

-- somehow i need it even though it's never used
data User = User
  { _user :: Text
  , _pass :: Text
  } deriving (Eq, Show)


type RedditSavedPostsAPI = "user" :> Capture "username" Text :> "saved"
                        :> QueryParam "limit" Int
                        :> Header "User-Agent" Text 
                        :> Header "Authorization" Text 
                        :> Get '[JSON] Listing 
