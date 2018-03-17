
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model ( Topic(..)
             , Comment(..)
             , EntityField (..)
             , doMigrations
             , runDB
             ) where

import Control.Monad.Reader     (MonadIO, asks, liftIO, ReaderT)

import Database.Persist         ((==.))
import Database.Persist.Class   (PersistEntity(..))
import Database.Persist.Sql     (runMigration, runSqlPool, SqlBackend)
import Database.Persist.TH      (mkMigrate, mkPersist, persistLowerCase,
                                 share, sqlSettings)

import Data.Text                (Text)

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Topic json
    title Text
    deriving Show
Comment json
    comment Text
    topicId TopicId
    deriving Show
|]

doMigrations :: MonadIO m => ReaderT SqlBackend m ()
doMigrations = runMigration migrateAll

runDB :: MonadIO m => ReaderT SqlBackend IO a -> ReaderT Config m a
runDB command = do
    pool <- asks getPool
    liftIO $ runSqlPool command pool

