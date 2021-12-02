
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

module TicTacToe.Models where

    
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T
import Text.Read (readEither)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    email String
    password String
    UniqueEmail email
    deriving Show
Player
    userId UserId
    gameId GameId
    playerCode String
    playOrder Int
    UniqueGameAndCode gameId playerCode
    UniqueGameOrder gameId playOrder
    deriving Show
Game
    gameCode String
    size Size
    UniqueGameCode gameCode
    deriving Show
Move
    playerId PlayerId
    gameId GameId
    movePos Position
    UniqueMove gameId movePos
    deriving Show
|]

data Size = Size Int Int deriving Show

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

data Position = Position Int Int deriving Show

instance PersistFieldSql Position where
    sqlType _ = SqlString

instance PersistField Position where
    toPersistValue (Position x y) = PersistText $ T.pack $ show x <> "," <> show y
    fromPersistValue = parsePair Position