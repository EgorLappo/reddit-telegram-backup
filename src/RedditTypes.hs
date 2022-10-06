module RedditTypes
  ( Item
  , AccessToken(..)
  , RLinkData(..)
  , Listing(..)
  , getListingItems
  ) where

import Data.Either
import GHC.Generics ( Generic )
import Data.Text (Text, unpack)

import Data.Aeson
import Data.Aeson.TH

-- TYPES FOR AUTHENTICATION

data AccessToken = AccessToken
  { access_token :: Text
  , expires_in :: Int
  , scope :: Text
  , token_type :: Text
  } deriving (Show, Generic)

instance FromJSON AccessToken
instance ToJSON AccessToken

-- TYPES FOR PARSING LISTINGS OF SAVED ITEMS
type Item = RLinkData

data RLinkData = RLinkData
  { subreddit   :: Text
  , author      :: Text
  , title       :: Text
  , selftext    :: Text
  , url         :: Text
  , created_utc :: Int
  } deriving (Show, Generic)

data RCommentData = RCommentData
  { comment_subreddit :: Text
  , comment_author    :: Text
  , body              :: Text
  , link_permalink    :: Text
  , comment_created_utc :: Int
  } deriving (Show, Generic)

data RLink = RLink
  { link_data :: Value
  , link_kind :: Text
  } deriving (Show, Generic)

data ListingData = ListingData
  { after    :: Maybe Text
  , before   :: Maybe Text
  , children :: [RLink]
  } deriving (Show, Generic)

newtype Listing = Listing
  { listing_data :: ListingData
  } deriving (Show, Generic)

-- derive aeson typeclasses avoiding name clashes between types and with prelude
instance FromJSON RLinkData
instance ToJSON RLinkData
$(deriveJSON defaultOptions {fieldLabelModifier = \case {"comment_subreddit" -> "subreddit"; "comment_author" -> "author"; "comment_created_utc" -> "created_utc"; name -> name} } ''RCommentData)
$(deriveJSON defaultOptions {fieldLabelModifier = \case {"link_data" -> "data"; "link_kind" -> "kind"; name -> name} } ''RLink)
instance FromJSON ListingData
instance ToJSON ListingData
$(deriveJSON defaultOptions {fieldLabelModifier = \case {"listing_data" -> "data"; name -> name} } ''Listing)

-- extract links from the listing, 
-- parse them depending on the type (post/comment)
-- convert comments to links, 
-- return a list of Parsec results
getListingItems :: Listing -> [Result Item]
getListingItems = joinResults . parsePostsAndComments . children . listing_data
  where joinResults (ps, cs) = ps ++ ((commentToLink <$>) <$> cs)

commentToLink :: RCommentData -> RLinkData
commentToLink (RCommentData s a b u t) = RLinkData s a ("Comment in r/" <> s) b u t

parsePostsAndComments :: [RLink] -> ([Result RLinkData], [Result RCommentData])
parsePostsAndComments = partitionEithers
                         . map (\(RLink val k) -> case k of
                                    "t3" -> Left $ fromJSON val
                                    "t1" -> Right $ fromJSON val
                                    t    -> Left $ Error $ "object type mismatch in parsing saved items: "<> unpack t)