module Bluesky.Handle
  ( Handle, rawHandle, makeHandle, HandleError(..), validTld
  ) where

import qualified Data.Bifunctor as Bifunctor
import Data.Char
import qualified Data.Text as Text
import Data.Text (Text)
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
validTld (Handle h) = case Text.breakOnEnd "." h of
  (_, tld) -> tld `notElem`
    [ "alt"
    , "arpa"
    , "example"
    , "internal"
    , "invalid"
    , "local"
    , "localhost"
    , "onion"
    ]

instance FromHttpApiData Handle where
  parseUrlPiece = Bifunctor.first (Text.pack . show) . makeHandle
