-- | @servant-match@ provides a standalone implementation of
-- dispatching a function by matching an `URI` against a servant
-- API. This can be used to parse the `URI` into a more structured
-- representation. For example:
--
-- @
-- data DataView
--   = Show
--   | Edit
--   deriving (Show, Eq)
--
-- data View
--   = ViewUsers
--   | ViewNewUser
--   | ViewUser !Text !DataView
--   deriving (Show, Eq)
--
-- data User = User
--
-- type API =
--   "users" :> (Get '[JSON] [User] :\<|> "new" :> ReqBody '[JSON] User :> Post '[JSON] User) :\<|>
--   "user" :> Capture "user" Text :>
--     (Get '[JSON] User :\<|>
--      "edit" :> ReqBody '[JSON] User :> Put '[JSON] User)
--
-- parser :: MatchT API View
-- parser =
--   (ViewUsers :\<|> ViewNewUser) :\<|> (\u -> ViewUser u Show :\<|> ViewUser u Edit)
--
-- parseView :: URI -> Maybe View
-- parseView u = matchURI (Proxy :: Proxy API) parser u
-- @
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Match
  ( matchURI
  , uriToLocation
  , Matches(..)
  , Location(..)
  ) where

import           Control.Applicative
import           Data.Bifunctor
import qualified Data.ByteString.UTF8 as UTF8
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.TypeLits
import           Network.HTTP.Types (parseQuery, decodePathSegments)
import           Network.URI hiding (query)
import           Servant.API

data Location = Location
  { segments :: [Text]
  , query :: [(String, Maybe String)]
  , isSecure :: IsSecure
  } deriving (Show, Eq, Ord)

class Matches api where
  type MatchT api r :: *
  matchLocation :: Proxy api -> MatchT api a -> Location -> Maybe a

instance (KnownSymbol s, Matches sub) => Matches (s :> sub) where
  type MatchT (s :> sub) r = MatchT sub r
  matchLocation _ p l =
    case segments l of
      [] -> Nothing
      (s':ss)
        | s' == Text.pack (symbolVal (Proxy :: Proxy s)) ->
          matchLocation (Proxy :: Proxy sub) p l {segments = ss}
        | otherwise -> Nothing

instance (Matches a, Matches b) => Matches (a :<|> b) where
  type MatchT (a :<|> b) r = MatchT a r :<|> MatchT b r
  matchLocation _ (a :<|> b) u =
    matchLocation (Proxy :: Proxy a) a u <|>
    matchLocation (Proxy :: Proxy b) b u

instance Matches EmptyAPI where
  type MatchT EmptyAPI r = EmptyAPI
  matchLocation _ _ _ = Nothing

instance (Matches sub, FromHttpApiData a) =>
         Matches (Capture sym a :> sub) where
  type MatchT (Capture sym a :> sub) r = a -> MatchT sub r
  matchLocation _ f l =
    case segments l of
      [] -> Nothing
      (s:ss) ->
        case parseUrlPiece s of
          Left _ -> Nothing
          Right a -> matchLocation (Proxy :: Proxy sub) (f a) l {segments = ss}

instance (Matches sub, FromHttpApiData a) =>
         Matches (CaptureAll sym a :> sub) where
  type MatchT (CaptureAll sym a :> sub) r = [a] -> MatchT sub r
  matchLocation _ f l =
    case traverse parseUrlPiece (segments l) of
      Left _ -> Nothing
      Right as -> matchLocation (Proxy :: Proxy sub) (f as) l {segments = []}

instance Matches sub => Matches (Header sym a :> sub) where
  type MatchT (Header sym a :> sub) r = MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) p l

instance Matches sub => Matches (HttpVersion :> sub) where
  type MatchT (HttpVersion :> sub) r = MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) p l

instance (KnownSymbol sym, Matches sub) => Matches (QueryFlag sym :> sub) where
  type MatchT (QueryFlag sym :> sub) r = Bool -> MatchT sub r
  matchLocation _ p l =
    let param =
          case lookup paramname (query l) of
            Just Nothing -> True
            Just (Just v) -> examine v
            Nothing -> False
    in matchLocation (Proxy :: Proxy sub) (p param) l
    where
      paramname = symbolVal (Proxy :: Proxy sym)
      examine v
        | v == "true" || v == "1" || v == "" = True
        | otherwise = False

instance (KnownSymbol sym, FromHttpApiData a, Matches sub) =>
         Matches (QueryParam sym a :> sub) where
  type MatchT (QueryParam sym a :> sub) r = Maybe a -> MatchT sub r
  matchLocation _ p l =
    case lookup paramname (query l) of
      Nothing -> matchLocation (Proxy :: Proxy sub) (p Nothing) l
      Just Nothing -> matchLocation (Proxy :: Proxy sub) (p Nothing) l
      Just (Just v) ->
        case parseQueryParam (Text.pack v) of
          Left e -> Nothing
          Right param -> matchLocation (Proxy :: Proxy sub) (p (Just param)) l
    where
      paramname = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, FromHttpApiData a, Matches sub) =>
         Matches (QueryParams sym a :> sub) where
  type MatchT (QueryParams sym a :> sub) r = [a] -> MatchT sub r
  matchLocation _ p l =
    case traverse (parseQueryParam . Text.pack) params of
      Left _ -> Nothing
      Right as -> matchLocation (Proxy :: Proxy sub) (p as) l
    where
      paramname = symbolVal (Proxy :: Proxy sym)
      params :: [String]
      params = mapMaybe snd . filter (looksLikeParam . fst) $ query l
      looksLikeParam name = name == paramname || name == (paramname <> "[]")

instance Matches sub => Matches (ReqBody contentTypes a :> sub) where
  type MatchT (ReqBody contentTypes a :> sub) r = MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) p l

instance Matches sub => Matches (RemoteHost :> sub) where
  type MatchT (RemoteHost :> sub) r = MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) p l

instance Matches sub => Matches (IsSecure :> sub) where
  type MatchT (IsSecure :> sub) r = IsSecure -> MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) (p (isSecure l)) l

instance Matches sub => Matches (Vault :> sub) where
  type MatchT (Vault :> sub) r = MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) p l

instance Matches sub => Matches (WithNamedContext name subContext sub) where
  type MatchT (WithNamedContext name subContext sub) r = MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) p l

instance Matches (Verb method statusCode contentTypes a) where
  type MatchT (Verb method statusCode contentTypes a) r = r
  matchLocation _ p l =
    case segments l of
      [] -> Just p
      _ -> Nothing

instance Matches sub => Matches (BasicAuth realm userData :> sub) where
  type MatchT (BasicAuth realm userData :> sub) r = MatchT sub r
  matchLocation _ p l = matchLocation (Proxy :: Proxy sub) p l

uriToLocation :: URI -> Location
uriToLocation u =
  Location
  { segments = (decodePathSegments . UTF8.fromString . uriPath) u
  , query =
      (map (bimap UTF8.toString (fmap UTF8.toString)) .
       parseQuery . UTF8.fromString . uriQuery)
        u
  , isSecure =
      if uriScheme u == "https:"
        then Secure
        else NotSecure
  }

matchURI :: Matches api => Proxy api -> MatchT api a -> URI -> Maybe a
matchURI proxy parser u = matchLocation proxy parser (uriToLocation u)
