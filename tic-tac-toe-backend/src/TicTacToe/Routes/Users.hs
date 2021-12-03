module TicTacToe.Routes.Users where

import Data.Text.Lazy (Text)
import qualified Web.Scotty.Trans as S
import TicTacToe.Models
import qualified Database.Persist as P
import Control.Monad.Reader (lift)
import Database.Persist.Sql (SqlPersistM)
import Network.HTTP.Types.Status (created201, internalServerError500, notFound404)
import Data.Aeson

routes :: S.ScottyT Text SqlPersistM ()
routes = do
    S.get "/1/users" $ do
        users <- lift (P.selectList [] [])

        S.json $ (users :: [P.Entity User])

    S.post "/1/users" $ do
        user <- S.jsonData
        k <- lift $ P.insert (user :: User)
        S.status created201
        S.json $ object [ "id" .= k ]

    S.get "/1/users/:id" $ do
        id <- S.param "id"
        mUser <- lift $ P.get (toKey id)
        case mUser of
            Nothing -> userNotFound
            Just user -> S.json (user :: User)

    S.delete "/1/users/:id" $ do
        id <- S.param "id"
        lift $ P.delete (toKey id :: UserId)
        S.text "Success!"

userNotFound = do
    S.status notFound404
    S.json $ object [ "error" .= ("User not found." :: String) ]