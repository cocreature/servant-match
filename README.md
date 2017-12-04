# servant-match

[![Travis](https://img.shields.io/travis/cocreature/servant-match.svg)](https://travis-ci.org/cocreature/servant-match)
[![Hackage](https://img.shields.io/hackage/v/servant-match.svg)](https://hackage.haskell.org/package/servant-match)

This package provides a standalone implementation of dispatching a
function by matching it against a Servant API. A common usecase for
this is to convert an `URI` to an ADT that provides a more structured
representation of the URL.

## Usage

```haskell
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
```
