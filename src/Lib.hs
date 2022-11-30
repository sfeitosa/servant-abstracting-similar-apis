{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data Product = Product 
  { 
    productId :: Int
  , productDescr :: String
  }

$(deriveJSON defaultOptions ''Product)

type API = "users" :> APIFor User Int
      :<|> "products" :> APIFor Product Int 

type APIFor a i = Get '[JSON] [a]
             :<|> ReqBody '[JSON] a :> Post '[JSON] i
             :<|> Capture "id" i :> Get '[JSON] a 
             :<|> Capture "id" i :> ReqBody '[JSON] a :> PutNoContent
             :<|> Capture "id" i :> DeleteNoContent

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API 
server = serverFor getUsers newUser getUser updateUser deleteUser
    :<|> serverFor getProducts newProduct getProduct updateProduct deleteProduct

serverFor :: Handler [a] 
          -> (a -> Handler i)
          -> (i -> Handler a)
          -> (i -> a -> Handler NoContent)
          -> (i -> Handler NoContent)
          -> Server (APIFor a i)
serverFor listAll new view update delete = listAll :<|> new :<|> view :<|> update :<|> delete

getUsers :: Handler [User]
getUsers = return users 

getUser :: Int -> Handler User
getUser _ = return (head users)

newUser :: User -> Handler Int 
newUser _ = return 100

updateUser :: Int -> User -> Handler NoContent
updateUser _ _ = return NoContent

deleteUser :: Int -> Handler NoContent
deleteUser _ = return NoContent

getProducts :: Handler [Product]
getProducts = return products 

getProduct :: Int -> Handler Product 
getProduct _ = return (head products)

newProduct :: Product -> Handler Int 
newProduct _ = return 101

updateProduct :: Int -> Product -> Handler NoContent
updateProduct _ _ = return NoContent

deleteProduct :: Int -> Handler NoContent
deleteProduct _ = return NoContent

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

products :: [Product]
products = [ Product 1 "Rice"
           , Product 2 "Beans"
           ]
