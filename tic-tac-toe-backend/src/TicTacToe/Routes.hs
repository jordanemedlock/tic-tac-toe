module TicTacToe.Routes where

import Web.Scotty.Trans
import Data.Text.Lazy (Text)
import qualified TicTacToe.Routes.Games as Games
import qualified TicTacToe.Routes.Moves as Moves
import qualified TicTacToe.Routes.Players as Players
import qualified TicTacToe.Routes.Users as Users
import Database.Persist.Sql (SqlPersistM)

allRoutes :: ScottyT Text SqlPersistM ()
allRoutes = do
    get "/1/" $ do
        text "Hello World!"
    Games.routes
    Moves.routes
    Players.routes
    Users.routes