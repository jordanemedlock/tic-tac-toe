module TicTacToe.Routes.Players where

import Data.Text.Lazy (Text)
import qualified Web.Scotty.Trans as S
import TicTacToe.Models
import qualified Database.Persist as P

-- type Persist = ReaderT backend IO

routes :: S.ScottyT Text IO ()
routes = do
    S.get "/1/players" $ do
        S.text "hello world!"
        -- players <- lift P.selectList [] []

        -- S.json $ players :: [P.Entity Player]
