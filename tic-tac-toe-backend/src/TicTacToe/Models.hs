
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module TicTacToe.Models where

    
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T
import Text.Read (readEither)
import GHC.Generics
import Data.Aeson
import Control.Monad.Reader (ReaderT, lift)
import Database.Persist.SqlBackend.Internal (SqlBackend)
import Control.Monad.Logger (NoLoggingT)
import Conduit (ResourceT)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    email String
    password String
    UniqueEmail email
    deriving Show Generic
Player
    userId UserId
    gameId GameId
    code String
    order Int
    UniqueGameAndCode gameId code
    UniqueGameOrder gameId order
    deriving Show Generic
Game
    code String
    size Size
    UniqueGameCode code
    deriving Show Generic
Move
    playerId PlayerId
    gameId GameId
    pos Position
    UniqueMove gameId pos
    deriving Show Generic
|]

instance FromJSON User
instance ToJSON User
instance FromJSON Player
instance ToJSON Player 
instance FromJSON Game
instance ToJSON Game
instance FromJSON Move
instance ToJSON Move
instance FromJSON Size
instance ToJSON Size
instance FromJSON Position
instance ToJSON Position


data Size = Size Int Int deriving (Show, Generic)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = (Right x)

readPair f x y = mapLeft T.pack $ f <$> readEither (T.unpack x) <*> readEither (T.unpack y)
persistPair x y = PersistText $ T.pack $ show x <> "," <> show y
parsePair f (PersistText t) = case T.split (==',') t of
        [x, y] -> readPair f x y
        _ -> Left $ "Wrong format for value: " <> t
parsePair _ t = Left $ "Wrong format for value: " <> (T.pack $ show t)

instance PersistField Size where
    toPersistValue (Size x y) = persistPair x y
    fromPersistValue = parsePair Size
instance PersistFieldSql Size where
    sqlType _ = SqlString

data Position = Position Int Int deriving (Show, Generic)

instance PersistFieldSql Position where
    sqlType _ = SqlString

instance PersistField Position where
    toPersistValue (Position x y) = PersistText $ T.pack $ show x <> "," <> show y
    fromPersistValue = parsePair Position

instance FromJSON (Entity User) where
    parseJSON = entityIdFromJSON
instance ToJSON (Entity User) where
    toJSON = entityIdToJSON
instance FromJSON (Entity Player) where
    parseJSON = entityIdFromJSON
instance ToJSON (Entity Player) where
    toJSON = entityIdToJSON
instance FromJSON (Entity Game) where
    parseJSON = entityIdFromJSON
instance ToJSON (Entity Game) where
    toJSON = entityIdToJSON
instance FromJSON (Entity Move) where
    parseJSON = entityIdFromJSON
instance ToJSON (Entity Move) where
    toJSON = entityIdToJSON

type Persist = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

toKey :: ToBackendKey SqlBackend a => Integer -> Key a
toKey i = toSqlKey (fromIntegral i)