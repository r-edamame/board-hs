
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Logger (NoLoggingT(..))

import Network.Wai.Handler.Warp (run, Port)
import Database.Persist.Postgresql (withPostgresqlPool)

import API (app)
import Config

main :: IO ()
main = runNoLoggingT $ withPostgresqlPool "host=db port=5432 dbname=test user=postgres" 3
    $ \pool -> NoLoggingT $ run 3000 (app $ Config pool)
