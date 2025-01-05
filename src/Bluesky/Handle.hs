module Bluesky.Handle
  ( Handle, rawHandle, makeHandle, HandleError(..), validTld
  , Did, rawDid, resolveViaDns
  ) where

import qualified Data.Bifunctor as Bifunctor
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import qualified Network.DNS as DNS
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
