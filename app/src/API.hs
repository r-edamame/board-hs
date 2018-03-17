
{-# LANGUAGE TypeOperators #-}

module API (app) where

import Servant      (serve, Proxy(..), (:<|>)(..), serveDirectoryFileServer
                    , Raw, Server, Application)

import API.Comment

import Config

type AppAPI = CommentAPI :<|> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

files :: Server Raw
files = serveDirectoryFileServer "public"

app :: Config -> Application
app cfg = serve appAPI (commentServer cfg :<|> files)

