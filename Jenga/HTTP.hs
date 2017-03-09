{-# LANGUAGE OverloadedStrings #-}
module Jenga.HTTP where

import           Control.Monad (when, void)

import           Data.Aeson (eitherDecode')
import qualified Data.Text as T

import           Jenga.PackageList
import           Jenga.Stack

import           Network.HTTP.Client.Conduit
import           Network.HTTP.Types.Header (hAccept)
import           Network.HTTP.Types.Status (status200)



stackageUrl :: String
stackageUrl = "https://www.stackage.org/"

-- Should use an ErrorT here.
getStackageResolverPkgList :: StackConfig -> IO (Either String PackageList)
getStackageResolverPkgList scfg = do
  -- TODO: swap out for something that doesn't `fail`.
  request <- parseRequest $ stackageUrl ++ T.unpack (stackResolver scfg)
  withManager $ do
    response <- httpLbs $ request { requestHeaders = (hAccept, "application/json") : requestHeaders request }

    when (responseStatus response /= status200) $
      void . error $ "getStackageResolverPkgList: status " ++ show (responseStatus response)

    pure $ eitherDecode' (responseBody response)
