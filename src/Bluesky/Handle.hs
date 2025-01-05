module Bluesky.Handle
  ( Handle, rawHandle, makeHandle, HandleError(..), validTld
  , Did, rawDid, resolveViaDns, resolveViaHttp
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Bifunctor as Bifunctor
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import qualified Network.DNS as DNS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

-- | https://atproto.com/specs/handle
newtype Handle = Handle { rawHandle :: Text }
  deriving stock (Eq, Ord, Show)

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

-- | https://atproto.com/specs/did
newtype Did = Did { rawDid :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

-- | Returns 'Nothing' in ordinary cases where this handle can't be resolved by
-- DNS. May raise an exception if either the handle has an invalid TLD or
-- something goes wrong with DNS resolution.
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
          pure (Just (Did rawDid))
        Right [] -> pure Nothing
        Left DNS.NameError -> pure Nothing
        other -> error (show other)

-- | Returns 'Nothing' when the expected hostname reports 404 for the HTTP
-- resolution endpoint. May raise an exception if either the handle has an
-- invalid TLD or there's no HTTP server at the expected domain at all.
--
-- Note that this handle shouldn't be considered valid for this DID until you've
-- looked up the associated DID document and checked it appears there.
resolveViaHttp :: HTTP.Manager -> Handle -> IO (Maybe Did)
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
