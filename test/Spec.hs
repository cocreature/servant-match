{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Servant.API
import Test.Hspec
import Data.Text (Text)
import Data.Proxy
import Network.URI

import Servant.Match

matchURI' :: Matches api => Proxy api -> MatchT api a -> String -> Maybe a
matchURI' proxy parser s = case
  parseURI s of
    Just u -> matchURI proxy parser u
    Nothing -> Nothing

main :: IO ()
main = hspec $ do
  describe "matchURI" $ do
    it "matches /users" $
      match "http://example.com/users" `shouldBe` Just ViewUsers
    it "matches /users/new" $
      match "http://example.com/users/new" `shouldBe` Just ViewNewUser
    it "matches /user/johndoe" $
      match "http://example.com/user/johndoe" `shouldBe` Just (ViewUser "johndoe" Show)
    it "matches /user/johndoe/edit" $
      match "http://example.com/user/johndoe/edit" `shouldBe` Just (ViewUser "johndoe" Edit)
  where match = matchURI' (Proxy :: Proxy API) parser

data DataView
  = Show
  | Edit
  deriving (Show, Eq)

data View
  = ViewUsers
  | ViewNewUser
  | ViewUser !Text !DataView
  deriving (Show, Eq)

data User = User

type API =
  "users" :> (Get '[JSON] [User] :<|> "new" :> ReqBody '[JSON] User :> Post '[JSON] User) :<|>
  "user" :> Capture "user" Text :>
    (Get '[JSON] User :<|>
     "edit" :> ReqBody '[JSON] User :> Put '[JSON] User)

parser :: MatchT API View
parser =
  (ViewUsers :<|> ViewNewUser) :<|> (\u -> ViewUser u Show :<|> ViewUser u Edit)
