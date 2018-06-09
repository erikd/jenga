{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jenga.HTTP
  ( getStackageResolverPkgList
  ) where

import           Control.Monad.Catch (Handler (..))
import           Control.Monad.Trans.Either (EitherT, firstEitherT, handleIOEitherT
                                            , handlesEitherT, hoistEither, left)

import           Data.Aeson (eitherDecode')
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text

import           Jenga.PackageList
import           Jenga.Stack
import           Jenga.Types

import           Network.HTTP.Simple (HttpException, Request)
import qualified Network.HTTP.Simple as Http
import           Network.HTTP.Types.Header (hAccept)
import           Network.HTTP.Types.Status (status200)



stackageUrl :: String
stackageUrl = "https://www.stackage.org/"


getStackageResolverPkgList :: StackConfig -> EitherT JengaError IO PackageList
getStackageResolverPkgList scfg = do
  request <- parseRequestEitherT scfg
  body <- runRequest request
  firstEitherT JengaJsonError $ hoistEither (eitherDecode' body)


runRequest :: Request -> EitherT JengaError IO ByteString
runRequest request =
  either left pure =<< handlesEitherT handlers action
  where
    action = do
      response <- Http.httpLbs request
      pure $
        if Http.getResponseStatus response /= status200
          then Left $ JengaHttpStatus "getStackageResolverPkgList: " (show $ Http.getResponseStatus response)
          else Right $ Http.getResponseBody response

    handlers =
      [ Handler (pure . JengaHttpIOError)
      , Handler (\(e :: HttpException) -> pure $ JengaHttpException (show e))
      ]


parseRequestEitherT :: StackConfig -> EitherT JengaError IO Request
parseRequestEitherT scfg = do
  fmap modify . handleIOEitherT handler $ Http.parseRequest stackUrl
  where
    stackUrl =
      stackageUrl ++ Text.unpack (stackResolver scfg)
    handler =
      const . JengaParseUrl $ Text.pack stackUrl
    modify req =
      Http.setRequestHeader hAccept ["application/json"] req
