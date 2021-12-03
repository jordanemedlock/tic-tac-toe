module TicTacToe.Routes.Players where

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
    S.get "/1/players" $ do
        players <- lift (P.selectList [] [])

        S.json $ (players :: [P.Entity Player])

    S.post "/1/players" $ do
        player <- S.jsonData
        k <- lift $ P.insert (player :: Player)
        S.status created201
        S.json $ object [ "id" .= k ]

    S.get "/1/players/:id" $ do
        id <- S.param "id"
        mPlayer <- lift $ P.get (toKey id)
        case mPlayer of
            Nothing -> playerNotFound
            Just player -> S.json (player :: Player)

    S.delete "/1/players/:id" $ do
        id <- S.param "id"
        lift $ P.delete (toKey id :: PlayerId)
        S.text "Success!"

playerNotFound = do
    S.status notFound404
    S.json $ object [ "error" .= ("Player not found." :: String) ]