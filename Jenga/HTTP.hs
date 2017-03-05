{-# LANGUAGE OverloadedStrings #-}
module Jenga.HTTP where

import Control.Monad (when, void)

import Data.Aeson (eitherDecode')

import Network.HTTP.Client.Conduit
import Network.HTTP.Types.Header (hAccept)
import Network.HTTP.Types.Status (status200)

import Jenga.PackageList


newtype StackResolver
  = StackResolver String
  deriving (Eq, Show)



stackageUrl :: String
stackageUrl = "https://www.stackage.org/"

-- Should use an ErrorT here.
getStackageResolverPkgList :: StackResolver -> IO (Either String PackageList)
getStackageResolverPkgList (StackResolver sr) = do
  -- TODO: swap out for something that doesn't `fail`.
  request <- parseRequest $ stackageUrl ++ sr
  withManager $ do
    response <- httpLbs $ request { requestHeaders = (hAccept, "application/json") : requestHeaders request }

    when (responseStatus response /= status200) $
      void . error $ "getStackageResolverPkgList: status " ++ show (responseStatus response)

    pure $ eitherDecode' (responseBody response)
