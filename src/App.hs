module App (app, runApp) where

import Control.Arrow ((>>>))
import Data.Proxy
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Polysemy
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Output

import Servant.API
import Servant.Client

import Config
import TelegramServant
import RedditServant
import RedditTypes
import ServantEffect
import DatabaseSQLite

-- EFFECTS     

-- get posts from reddit in some way
data RedditAPI m a where
    AuthenticateReddit :: RedditAPI m AccessToken
    GetSavedItemsReddit :: AccessToken -> RedditAPI m Listing

-- back them up to telegram
data Backup m a where
    BackupItem :: Item -> Backup m ()

-- keep a local database to prevent posting duplicates
data LocalStorage m a where
    GetMostRecentItem :: LocalStorage m (Maybe DBItem)
    TakeItems :: LocalStorage m [DBItem]
    PutItems :: [Item] -> LocalStorage m ()

makeSem ''RedditAPI
makeSem ''Backup
makeSem ''LocalStorage


-- main logic of the program
app 
  :: Members '[RedditAPI, LocalStorage, Backup, Error String, Output Text, Embed IO] r 
  => Sem r ()
app = do
    rToken <- authenticateReddit
    output $ "reddit authentication successful..."

    response <- getSavedItemsReddit rToken
    posts <- mapM fromResult $ getListingItems response
    output $ "got " <> T.pack (show $ length posts) <> "posts from Reddit..."

    latestPost <- getMostRecentItem
    let newPosts = reverse $ maybe posts (keepNewPosts posts) latestPost
    output $ T.pack (show $ length newPosts) <> " new posts detected..."

    output "backing up..."
    putItems newPosts
    mapM_ backupItem newPosts


runApp 
  :: Config
  -> Sem
     '[RedditAPI, LocalStorage, Backup, ServantClient, Reader Config,
       Error ClientError, Error String, Output Text, Error Text, Embed IO] ()
  -> IO (Either Text Text)
runApp config = runRedditWithServant
            >>> runStorageWithSQLite
            >>> runBackupWithServant
            >>> runServantClient
            >>> runReader config
            >>> mapError showErr
            >>> mapError textErr
            >>> runOutputMonoid id 
            >>> runError
            >>> runM
            >>> fmap (fmap fst)


runRedditWithServant
  :: Members '[ServantClient, Error ClientError, Reader Config] r
  => Sem (RedditAPI ': r) a -> Sem r a
runRedditWithServant m = do
    config <- asks redditConfig
    let authData = BasicAuthData (T.encodeUtf8 (clientID config)) (T.encodeUtf8 (clientSecret config))
    interpret (\case
        AuthenticateReddit   -> runClient (redditAuthBaseUrl config) $
          client (Proxy @RedditAuthAPI)
          authData
          (Just "password")
          (Just $ username config)
          (Just $ password config)
          (Just $ userAgent config)
        GetSavedItemsReddit t -> runClient (redditApiBaseUrl config) $
          client (Proxy @RedditSavedPostsAPI)
          (username config)
          (Just $ postLimit config)
          (Just $ userAgent config)
          (Just $ "bearer " <> access_token t)) m

runStorageWithSQLite
  :: Members '[Reader Config, Embed IO] r
  => Sem (LocalStorage ': r) a -> Sem r a
runStorageWithSQLite m = do
    config <- asks databaseConfig
    interpret (\case
      GetMostRecentItem -> embed $ do
        conn <- getConnection (databaseFile config)
        getMostRecentRow conn
      TakeItems -> embed $ do
        conn <- getConnection (databaseFile config)
        getAllRows conn
      PutItems xs -> embed $ do
        conn <- getConnection (databaseFile config)
        mapM_ (insertRow conn) xs) m

runBackupWithServant
  :: Members '[ServantClient, Error ClientError, Reader Config] r
  => Sem (Backup ': r) a -> Sem r a
runBackupWithServant m = do
  config <- asks telegramConfig
  interpret (\(BackupItem post) -> do
    runClient (tgApiBaseUrl config) $
      client (Proxy @TelegramBotAPI)
             (apiToken config)
             (Just $ chatId config)
             (Just $ prepareMessage post)
             (Just "html")
    pure ()) m

-- **** HELPER FUNCTIONS ****

-- function to convert 'Result Item' to Item or throw error
fromResult
  :: Member (Error String) r
  => Result a
  -> Sem r a
fromResult (Error s) = throw s
fromResult (Success a) = pure a

showErr :: ClientError -> String
showErr = show

textErr :: String -> Text 
textErr = T.pack

-- simple way to track which posts are new is to look at timestamp
-- the probability of timestamps matching is infinitely low
keepNewPosts :: [Item] -> DBItem -> [Item]
keepNewPosts xs y = takeWhile (\x -> created_utc x /= created_utc (redditLink y)) xs

prepareMessage :: Item -> Text
prepareMessage (RLinkData s a t _ u tst) =
  "<strong>" <> t <> "</strong>\n\n" <>
  "posted in r/" <> s <> " by <i>" <> a <> "</i> on " <> toDate tst <> "\n\n" <> u

toDate :: Int -> Text
toDate = T.pack . take 10 . iso8601Show . posixSecondsToUTCTime . realToFrac
