{-# LANGUAGE DeriveGeneric #-}

module User
  ( User(..), -- Export the User data type and its constructors
    CreateUserRequest(..) -- Export the CreateUserRequest data type and its constructors
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User
  { userId :: Text,
    userName :: Text
  }

-- Data type which describes the request that will be received to create a user
data CreateUserRequest = CreateUserRequest
  { name :: Text,
    password :: Text
  }
  deriving (Generic)

-- We define a FromJSON instance for CreateUserRequest because we will want
-- to parse it from an HTTP request body (JSON).
instance FromJSON CreateUserRequest