module TicTacToe.Routes.Moves where

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
    S.get "/1/moves" $ do
        moves <- lift (P.selectList [] [])

        S.json $ (moves :: [P.Entity Move])

    S.post "/1/moves" $ do
        move <- S.jsonData
        k <- lift $ P.insert (move :: Move)
        S.status created201
        S.json $ object [ "id" .= k ]

    S.get "/1/moves/:id" $ do
        id <- S.param "id"
        mMove <- lift $ P.get (toKey id)
        case mMove of
            Nothing -> moveNotFound
            Just move -> S.json (move :: Move)

    S.delete "/1/moves/:id" $ do
        id <- S.param "id"
        lift $ P.delete (toKey id :: MoveId)
        S.text "Success!"

moveNotFound = do
    S.status notFound404
    S.json $ object [ "error" .= ("Move not found." :: String) ]