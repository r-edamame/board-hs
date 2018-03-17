
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API.Comment ( CommentAPI
                   , commentServer
                   ) where

import Database.Persist             ((==.))
import Database.Persist.Sql         (toSqlKey)
import Database.Persist.Types       (Entity(..))
import Database.Persist.Class       (selectList, selectFirst, insert)
import Data.Int                     (Int64)
import Servant                      (Get, JSON, (:>), (:<|>)(..), Post, ReqBody, Capture, Server
                                    , Handler(..), QueryParam, err400, err404, hoistServer, Proxy(..), ServantErr)
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Except   (throwE)
import Control.Monad.Trans.Reader   (ReaderT(..))

import Model                        (Topic(..), Comment(..), runDB, EntityField(..))

import Config

type API' =
         "topics" :> Get '[JSON] [Entity Topic]
    :<|> "topic" :> QueryParam "topicId" Int64 :> Get '[JSON] (Entity Topic)
    :<|> "comments" :> QueryParam "topicId" Int64 :> Get '[JSON] [Entity Comment]
    :<|> "comments" :> ReqBody '[JSON] Comment :> Post '[JSON] [Entity Comment]
    :<|> "topics" :> "create" :>  ReqBody '[JSON] Topic :> Post '[JSON] (Entity Topic)

type CommentAPI = "api" :> API'

commentServer :: Config -> Server CommentAPI
commentServer cfg = hoistServer proxy (flip runReaderT cfg) (topics :<|> topic :<|> comments :<|> postComment :<|> createTopic)
    where
        proxy = Proxy :: Proxy CommentAPI


throw :: ServantErr -> ReaderT Config Handler a
throw = lift . Handler . throwE


topics :: ReaderT Config Handler [Entity Topic]
topics = runDB $ selectList [] []

topic :: Maybe Int64 -> ReaderT Config Handler (Entity Topic)
topic Nothing = throw err400
topic (Just tid) = do
    mtopic <- runDB $ selectFirst [ TopicId ==. toSqlKey tid ] []
    case mtopic of
        Just entity -> return entity
        Nothing -> throw err404

comments :: Maybe Int64 -> ReaderT Config Handler [Entity Comment]
comments (Just tid) = runDB $ selectList [ CommentTopicId ==. toSqlKey tid ] []
comments Nothing = throw err400

postComment :: Comment -> ReaderT Config Handler [Entity Comment]
postComment comment@(Comment text tid) = do
    mtopic <- runDB $ selectFirst [ TopicId ==. tid ] []
    case mtopic of
        Nothing -> throw err404
        Just _ -> do
            runDB $ insert $ comment
            runDB $ selectList [ CommentTopicId ==. tid ] []

createTopic :: Topic -> ReaderT Config Handler (Entity Topic)
createTopic topic = do
    tid <- runDB $ insert topic
    return $ Entity tid topic
