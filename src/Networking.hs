module Networking ( get
                  , getBin
                  ) where

import Network.HTTP ( simpleHTTP
                    , getRequest
                    , getResponseBody
                    , Request
                    , rqBody
                    , HStream
                    )

import qualified Data.ByteString as B

type URL = String

getHelper :: HStream b => (Request String -> Request b) -> URL -> IO b
getHelper fn = (getResponseBody =<<) . simpleHTTP . fn . getRequest

get :: URL -> IO String
get = getHelper id

getBin :: URL -> IO B.ByteString
getBin = getHelper (\r -> r {rqBody = mempty})
