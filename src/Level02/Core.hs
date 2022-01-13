{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request (requestBody), Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody, consumeRequestBodyStrict)
import           Network.Wai.Handler.Warp (run, InvalidRequest)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            (ContentType (PlainText), Error (InvalidTopic, InvalidCommentText, InvalidRequest), RqType (AddRq, ViewRq, ListRq), Topic,
                                           mkCommentText, mkTopic,
                                           renderContentType)
import Data.Text.Lazy.Lens (text)
import qualified Data.Text as Text

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType =
  responseLBS
  status [("Content-Type", renderContentType contentType)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 contentType =
  responseLBS status200 [("Content-Type", renderContentType contentType)]

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 contentType =
  responseLBS status404 [("Content-Type", renderContentType contentType)]

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 contentType =
  responseLBS status400 [("Content-Type", renderContentType contentType)]

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

-- mkAddRequest
--   :: Text
--   -> LBS.ByteString
--   -> Either Error RqType
-- mkAddRequest topic text =
--   fmap (\t -> AddRq t (lazyByteStringToStrictText text)) (mkTopic topic)
--   -- (\t -> AddRq t (lazyByteStringToStrictText text)) <$> mkTopic topic
--   where
--     -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
--     lazyByteStringToStrictText =
--       decodeUtf8 . LBS.toStrict

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topicText byteString =
  let
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict
    commentBody = lazyByteStringToStrictText byteString
  in
    fmap (\topic -> AddRq topic commentBody) (mkTopic topicText)


mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic =
  fmap ViewRq (mkTopic topic)


mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse InvalidTopic = responseLBS status404 [("Content-Type", "text/plain")] "Invalid topic"
mkErrorResponse InvalidCommentText = responseLBS status400 [("Content-Type", "text/plain")] "Empty Comment Text"
mkErrorResponse InvalidRequest = responseLBS status404 [("Content-Type", "text/plain")] "Request type not found"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest request =
  let
    -- Can I extract this?
    -- case pathInfo Request of
    --   ["topics"] -> mkListRequest
    --   ["topic", topic] -> mkViewRequest topic
    --   ["post", topic] -> mkAddRequest topic requestBody Request
    --   _ -> InvalidRequest
  in
    fmap (\body -> case pathInfo request of
      ["list"] -> mkListRequest
      [topic, "view"] -> mkViewRequest topic
      [topic, "add"] -> mkAddRequest topic body
      _ -> Left InvalidRequest) (consumeRequestBodyStrict request)

  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ commentText) = if Text.length commentText > 0 then Right $ resp200 PlainText "AddRq not implemented yet" else Left InvalidCommentText
handleRequest (ViewRq _)  = Right $ resp200 PlainText "ViewRq not implemented yet"
handleRequest ListRq = Right $ resp200 PlainText "ListRq not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app request cb =
  let
    -- response = fmap (\requestType -> case handleRequest requestType of
    --   Left e -> mkErrorResponse e
    --   Right x -> x) ()
    errorOrResponseIO = fmap (>>= handleRequest) (mkRequest request)
    response = fmap (\ case
      Left e -> mkErrorResponse e
      Right x -> x) errorOrResponseIO
    -- Can I use do here?
    -- do
    --   errorOrResponse <- fmap (>>= handleRequest) (mkRequest request)
    --   response <- case errorOrResponse of
    --     Left e -> mkErrorResponse e
    --     Right x -> x
    --   cb response
  in
    response >>= cb

runApp :: IO ()
runApp = run 3000 app
