# Reddit-to-Telegram backup

A simple script to forward new saved posts from Reddit into a Telegram chat/channel written in Haskell.

This was an educational project, in which I tried to use the [`Polysemy`](https://hackage.haskell.org/package/polysemy) effect system. `Polysemy` allows you to specify any computational effects as datatypes, and then interpret each effect in whichever way you want. 

In this program, I have three such effects: the first one sends a query to Reddit API to get a list of recent saved posts; the second one saves newly added posts to some local storage; and the third one makes requests to Telegram Bot API to send messages to a Telegram chat. Effects are defined as GADT's (generalized algebraic datatypes), and Template Haskell is used to generate placeholder "functions" that will later be interpreted. In a simplified version, here is how this looks in code:

```haskell
-- define effects as data
data RedditAPI m a where
    AuthenticateReddit :: RedditAPI m AccessToken
    GetSavedItemsReddit :: AccessToken -> RedditAPI m Listing

data Backup m a where
    BackupItem :: Item -> Backup m ()

data LocalStorage m a where
    GetMostRecentItem :: LocalStorage m (Maybe DBItem)
    TakeItems :: LocalStorage m [DBItem]
    PutItems :: [Item] -> LocalStorage m ()

-- use TH to generate boilerplate
makeSem ''RedditAPI
makeSem ''Backup
makeSem ''LocalStorage

-- main logic of the program
app :: Members '[RedditAPI, LocalStorage, Backup, Error String, Output Text, Embed IO] r => Sem r ()
app = do
    authToken <- authenticateReddit
    response <- getSavedItemsReddit authToken
    posts <- getListingItems response
    latestPost <- getMostRecentItem
    let newPosts = keepNew posts latestPost
    putItems $ reverse newPosts
    mapM_ backupItem newPosts
```

After writing the main logic of the program, I chose to interpret Reddit and Telegram effects with one of the specialized packages called [`Servant`](https://hackage.haskell.org/package/servant). However, any other API-"backend" can be used without changing anything in the main code. For my local storage, I chose SQLite, with [`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple) as an adapter, with the potential of switching to a more powerful system later on.
Here is how this looks in practice, using SQLite as an example:

```haskell
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
```
Here, I essentially map the GADT's from above into specific monadic actions in the IO monad. Notice that I also use a `Reader`, which is an effect that is implemented within `Polysemy` and doesn't need interpretation in terms of IO.



## Setup and usage

1 - Register a new Reddit app at [https://ssl.reddit.com/prefs/apps/](https://ssl.reddit.com/prefs/apps/), note the client IO and secret.
2 - Register a new telegram bot by following instructions at [https://core.telegram.org/api](https://core.telegram.org/api), note the bot API token.
3 - Fill in the tokens and other registration data fields in `src/Config.hs`.
4 - Compile the project using `stack build`.
5 - Set the executable to run regularly (e.g. daily) using your preferred method.

## TODOS

1 - Read config from a `.yaml` file instead of a haskell source file
2 - Add better modularity and control of formatting options for telegram messages
3 - Save text of long posts as a [telegra.ph](https://telegra.ph) page and attach it to a message.
4 - Add a CLI to the script to be able to specify config/inspect the database/search posts.


