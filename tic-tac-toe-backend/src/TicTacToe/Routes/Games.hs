module TicTacToe.Routes.Games where

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
    S.get "/1/games" $ do
        games <- lift (P.selectList [] [])

        S.json $ (games :: [P.Entity Game])

    S.post "/1/games" $ do
        game <- S.jsonData
        k <- lift $ P.insert (game :: Game)
        S.status created201
        S.json $ object [ "id" .= k ]

    S.get "/1/games/:id" $ do
        id <- S.param "id"
        mGame <- lift $ P.get (toKey id)
        case mGame of
            Nothing -> gameNotFound
            Just game -> S.json (game :: Game)

    S.delete "/1/games/:id" $ do
        id <- S.param "id"
        lift $ P.delete (toKey id :: GameId)
        S.text "Success!"

gameNotFound = do
    S.status notFound404
    S.json $ object [ "error" .= ("Game not found." :: String) ]