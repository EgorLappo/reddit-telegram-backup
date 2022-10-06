module DatabaseSQLite 
  ( getConnection
  , insertRow
  , getAllRows
  , getMostRecentRow
  , DBItem(..)
  ) where

import Data.Maybe (listToMaybe)
import Data.Time.Clock.POSIX

import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

import RedditTypes

-- a row in our table is going to be everything in the link 
-- + the time it was added to the database 
data DBItem = DBItem 
  { redditLink :: Item
  , timeAdded :: Int
  } deriving (Show)

-- derive sqlite-simple typeclasses
instance ToRow DBItem where
  toRow (DBItem (RLinkData s a t st u c) time) = toRow (s,a,t,st,u,c,time)

instance FromRow DBItem where 
  fromRow = DBItem <$> (RLinkData <$> field <*> field <*> field <*> field <*> field <*> field) <*> field


getConnection :: Text -> IO Connection
getConnection file = do 
    conn <- open (T.unpack file)
    execute_ conn $ "CREATE TABLE IF NOT EXISTS posts "
                              <> "(subreddit TEXT, author TEXT, title TEXT, "
                              <> "selftext TEXT, url TEXT, created_utc INTEGER, inserted_utc INTEGER)"
    return conn

insertRow :: Connection -> Item -> IO ()
insertRow conn item = do
   timestamp <- round `fmap` getPOSIXTime 
   execute conn "INSERT INTO posts (subreddit, author, title, selftext, url, created_utc, inserted_utc) VALUES (?,?,?,?,?,?,?)" (DBItem item timestamp)

getAllRows :: Connection -> IO [DBItem]
getAllRows conn = query_ conn "SELECT * FROM posts ORDER BY created_utc DESC" :: IO [DBItem]

getMostRecentRow :: Connection -> IO (Maybe DBItem)
getMostRecentRow conn = listToMaybe <$> (query_ conn "SELECT * FROM posts ORDER BY created_utc DESC LIMIT 1" :: IO [DBItem])



