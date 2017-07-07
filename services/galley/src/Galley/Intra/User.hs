{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.User
    ( getConnections
    , deleteBot
    , reAuthUser
    , rmUsers
    ) where

import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Bilge.Retry
import Brig.Types.Intra (ConnectionStatus (..), ReAuthUser (..))
import Brig.Types.Connection (Relation (..))
import Galley.App
import Galley.Options
import Control.Monad (void, when)
import Control.Monad.Catch (throwM)
import Control.Lens (view)
import Control.Retry
import Data.ByteString.Char8 (pack, intercalate)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Id
import Data.List.NonEmpty (toList)
import Data.List1 (toNonEmpty, List1)
import Data.Misc (portNumber)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

import qualified Data.Text.Lazy as LT
import qualified Network.HTTP.Client.Internal as Http

getConnections :: UserId -> [UserId] -> Maybe Relation -> Galley [ConnectionStatus]
getConnections u uids rlt = do
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    r <- call "brig"
        $ method GET . host h . port (portNumber p)
        . path "/i/users/connections-status"
        . queryItem "users" users
        . maybe id rfilter rlt
        . expect2xx
    parseResponse (Error status502 "server-error") r
  where
    users   = intercalate "," $ toByteString' <$> u:uids
    rfilter = queryItem "filter" . (pack . map toLower . show)

deleteBot :: ConvId -> BotId -> Galley ()
deleteBot cid bot = do
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    void $ call "brig"
        $ method DELETE . host h . port (portNumber p)
        . path "/bot/self"
        . header "Z-Type" "bot"
        . header "Z-Bot" (toByteString' bot)
        . header "Z-Conversation" (toByteString' cid)
        . expect2xx

reAuthUser :: UserId -> ReAuthUser -> Galley Bool
reAuthUser uid auth = do
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    let req = method GET . host h . port (portNumber p)
            . paths ["/i/users", toByteString' uid, "reauthenticate"]
            . json auth
    st <- statusCode . responseStatus <$> call "brig" (check . req)
    return $ if st == 200 then True
                          else False
  where
    check :: Request -> Request
    check r = r { Http.checkResponse = \rq rs ->
        when (responseStatus rs `notElem` [status200, status403]) $
            let ex = StatusCodeException (rs { responseBody = () }) mempty
            in throwM $ HttpExceptionRequest rq ex
    }

rmUsers :: List1 UserId -> Galley ()
rmUsers uids = do
    h <- view (options.brigHost)
    p <- view (options.brigPort)
    void $ call "brig"
        $ method DELETE . host h . port (portNumber p)
        . path "/i/users"
        . queryItem "ids" users
        . expect2xx
  where
    users = intercalate "," $ toByteString' <$> toList (toNonEmpty uids)

-----------------------------------------------------------------------------
-- Helpers

call :: LT.Text -> (Request -> Request) -> Galley (Response (Maybe ByteString))
call n r = recovering x1 rpcHandlers (const (rpc n r))

x1 :: RetryPolicy
x1 = limitRetries 1
