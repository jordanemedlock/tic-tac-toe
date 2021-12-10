module Main where

import qualified Web.Scotty.Trans as S
import Data.Text.Lazy (Text)
import System.Console.ArgParser
import TicTacToe.Models
import TicTacToe.Routes
import Database.Persist.Sqlite
import Control.Monad.Reader (lift)
import Control.Monad.Logger (runNoLoggingT)


data Command = MigrateAll | RunServer deriving Show

commandParser = mkSubParser [ ("migrate-all", mkDefaultApp (pure MigrateAll) "migrate-all")
                            , ("run-server", mkDefaultApp (pure RunServer) "run-server")
                            ]

main :: IO ()
main = do
    parser <- commandParser
    runApp parser $ \case
        MigrateAll -> runSqlite "data.db" $ runMigration migrateAll
        RunServer -> runNoLoggingT $ do
            withSqlitePool "data.db" 10 $ \pool -> do
                S.scottyT 5000 (\m -> runSqlPersistMPool m pool) allRoutes

