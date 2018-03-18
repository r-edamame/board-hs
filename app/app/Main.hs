
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Logger (NoLoggingT(..))

import Network.Wai.Handler.Warp (run, Port)
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Postgresql (withPostgresqlPool)

import API (app)
import Config

import Model (doMigrations)

main :: IO ()
main = runNoLoggingT $ withPostgresqlPool "host=db port=5432 dbname=test user=postgres" 3
    $ \pool -> NoLoggingT $ do
                    runSqlPool doMigrations pool
                    run 3000 (app $ Config pool)
