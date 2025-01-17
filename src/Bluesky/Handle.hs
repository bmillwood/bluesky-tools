module Bluesky.Handle
  ( Handle, rawHandle, makeHandle, HandleError(..), validTld
  , resolveViaDns, resolveViaHttp, resolveViaBoth, BothFailed(..)
  , verifyHandle, resolveVerify
  ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Except
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor as Bifunctor
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import GHC.Generics
import GHC.Stack (HasCallStack)

import qualified Network.DNS as DNS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

import Bluesky.Did

-- | https://atproto.com/specs/handle
newtype Handle = Handle { rawHandle :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Aeson.ToJSON)

data HandleError
  = TooLong
  | BadCharacters
  | EmptySegment
  | SegmentTooLong
  | SegmentStartsWithHyphen
  | SegmentEndsWithHyphen
  | OnlyOneSegment
  | LastSegmentStartsWithNumber
  deriving stock (Eq, Ord, Show)

makeHandle :: Text -> Either HandleError Handle
makeHandle t
  | Text.length t > 253 = Left TooLong
  | otherwise = checkParts True t
  where
    checkParts firstCheck remaining = case Text.breakOn "." remaining of
      (before, after)
        | Text.null before -> Left EmptySegment
        | Text.length before > 63 -> Left SegmentTooLong
        | Text.any (not . segmentChar) before -> Left BadCharacters
        | Text.take 1 before == "-" -> Left SegmentStartsWithHyphen
        | Text.takeEnd 1 before == "-" -> Left SegmentEndsWithHyphen
        | Text.null after ->
          if firstCheck
          then Left OnlyOneSegment
          else if Text.all isNumber (Text.take 1 before)
          then Left LastSegmentStartsWithNumber
          else Right (Handle (Text.toLower t))
        | otherwise -> checkParts False (Text.drop 1 after)
    segmentChar c = isAscii c && isAlphaNum c || c == '-'

-- | See "Additonal Non-Syntax Restrictions" in the spec
validTld :: Handle -> Bool
validTld (Handle h) =
  all (\tld -> not (tld `Text.isSuffixOf` h))
    [ ".alt"
    , ".arpa"
    , ".example"
    , ".internal"
    , ".invalid"
    , ".local"
    , ".localhost"
    , ".onion"
    ]

instance FromHttpApiData Handle where
  parseUrlPiece = Bifunctor.first (Text.pack . show) . makeHandle

instance Aeson.FromJSON Handle where
  parseJSON =
    Aeson.withText "Bluesky.Handle.Handle" $ either (fail . show) pure . makeHandle

-- | Returns 'Nothing' in ordinary cases where this handle can't be resolved by
-- DNS. May raise an exception if:
--
-- * the handle has an invalid TLD,
-- * something goes wrong with DNS resolution,
-- * the DID returned is syntactically invalid.
--
-- Note that this handle shouldn't be considered valid for this DID until you've
-- looked up the associated DID document and checked it appears there.
resolveViaDns :: HasCallStack => Handle -> IO (Maybe Did)
resolveViaDns handle@(Handle rawHandle)
  | not (validTld handle) = error "handle has invalid TLD"
  | otherwise = do
      -- should we share rs / the resolver between calls?
      rs <- DNS.makeResolvSeed DNS.defaultResolvConf
      results <- DNS.withResolver rs $ \resolver ->
        DNS.lookupTXT resolver ("_atproto." <> Text.encodeUtf8 rawHandle)
      case results of
        Right [Text.decodeASCII -> Text.stripPrefix "did=" -> Just rawDid] ->
          either (error . show) (pure . Just) $ makeDid rawDid
        Right [] -> pure Nothing
        Left DNS.NameError -> pure Nothing
        other -> error (show other)

-- | Returns 'Nothing' when the expected hostname reports 404 for the HTTP
-- resolution endpoint. May raise an exception if either the handle has an
-- invalid TLD, the HTTP server doesn't return 200 or 404, or there's no HTTP
-- server at the expected domain at all. (This is probably a bit too strict, and
-- should ignore more HTTP errors, but I'll see based on my real-world
-- experience.)
--
-- Note that this handle shouldn't be considered valid for this DID until you've
-- looked up the associated DID document and checked it appears there.
resolveViaHttp :: HasCallStack => HTTP.Manager -> Handle -> IO (Maybe Did)
resolveViaHttp httpManager handle@(Handle rawHandle)
  | not (validTld handle) = error "handle has invalid TLD"
  | otherwise = do
      let rawHandleString = Text.unpack rawHandle
      req <-
        HTTP.parseRequest
          -- This could be a bad thing to do if the handle was arbitrary user
          -- data. But we validated it on the way in.
          -- (It still might be better to construct the URL in some structured way
          -- that insists the handle can only go in the hostname portion. But this
          -- will do.)
          ("https://" <> rawHandleString <> "/xrpc/com.atproto.identity.resolveHandle?handle=" <> rawHandleString)
      resp <- HTTP.httpLbs req httpManager
      case HTTP.statusCode (HTTP.responseStatus resp) of
        404 -> pure Nothing
        200 -> case Aeson.decode (HTTP.responseBody resp) of
          Nothing -> fail "JSON parsing failed"
          Just o -> either fail (pure . Just) $ Aeson.parseEither (.: "did") o
        other -> fail $ "Unexpected HTTP status " <> show other

-- | Raised by 'resolveViaBoth' when both methods raise exceptions.
data BothFailed = BothFailed
  { dnsException :: Except.SomeException
  , httpException :: Except.SomeException
  } deriving stock (Show)
    deriving anyclass (Except.Exception)

-- | If either 'resolveViaDns' or 'resolveViaHttp' return a 'Did', return that
-- 'Did'. Otherwise, if one or both of them raised an exception, reraise it (or
-- them, via 'BothFailed'). (Otherwise, return 'Nothing').
resolveViaBoth :: HasCallStack => HTTP.Manager -> Handle -> IO (Maybe Did)
resolveViaBoth httpManager handle =
  fromE =<< Async.concurrentlyE
    (toE $ resolveViaDns handle)
    (toE $ resolveViaHttp httpManager handle)
  where
    toE act = do
      r <- Except.try act
      case r of
        Right (Just did) -> pure (Left did)
        Right Nothing -> pure (Right Nothing)
        Left err -> pure (Right (Just err))
    fromE (Left r) = pure (Just r)
    fromE (Right (Nothing, Nothing)) = pure Nothing
    fromE (Right (Just e, Nothing)) = Except.throwIO e
    fromE (Right (Nothing, Just e)) = Except.throwIO e
    fromE (Right (Just dnsException, Just httpException)) =
      Except.throwIO BothFailed{ dnsException, httpException }

-- | @Just True@ if this 'Handle' appears in the DID 'Document' for the 'Did'.
-- @Just False@ if the document is available and doesn't affirm the handle.
-- 'Nothing' if the document can't be fetched.
verifyHandle :: HTTP.Manager -> Handle -> Did -> IO (Maybe Bool)
verifyHandle httpManager (Handle rawHandle) did = runMaybeT $ do
  doc <- MaybeT $ getDocument httpManager did
  pure $ ("at://" <> rawHandle) `elem` alsoKnownAs doc

-- | Combines 'resolveViaBoth' and 'verifyHandle'. Raises an error if
-- verification fails.
resolveVerify :: HasCallStack => HTTP.Manager -> Handle -> IO (Maybe Did)
resolveVerify httpManager handle = runMaybeT $ do
  did <- MaybeT $ resolveViaBoth httpManager handle
  verified <- lift $ verifyHandle httpManager handle did
  case verified of
    Nothing -> error "Can't get DID document to verify handle"
    Just False -> error "Handle failed verification: not in DID document"
    Just True -> pure did
