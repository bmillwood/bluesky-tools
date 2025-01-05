module Bluesky.Did
  ( Did(Did), rawDid
  , Document(Document, alsoKnownAs)
  , getDocument
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import GHC.Stack

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP

-- | https://atproto.com/specs/did
-- A DID is a Decentralized Identifier. They're codified by various W3C
-- standards. This type only aims to capture how they are used in atproto.
--
-- Currently we do not implement DID validation.
newtype Did = Did { rawDid :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

-- | The DID methods supported by atproto. Note that DIDs are used outside of
-- atproto and there are many more methods in those contexts, but we don't
-- support them here.
data Method
  = Web -- ^ https://w3c-ccg.github.io/did-method-web/
  | Plc -- ^ https://github.com/did-method-plc/did-method-plc
  deriving stock (Eq, Ord, Show)

method :: Did -> Maybe Method
method Did{ rawDid }
  | "did:web:" `Text.isPrefixOf` rawDid = Just Web
  | "did:plc:" `Text.isPrefixOf` rawDid = Just Plc
  | otherwise = Nothing

-- | Currently this is only used for ensuring a handle that has been resolved to
-- a DID is referenced by the DID document (so it is the correct handle for this
-- DID). The other fields are ignored.
data Document = Document
  { id :: Did
  , alsoKnownAs :: [Text]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON)

-- | This is currently only implemented for did:plc: DIDs.
getDocument :: HasCallStack => HTTP.Manager -> Did -> IO (Maybe Document)
getDocument httpManager did@(Did rawDid) =
  case method did of
    Nothing -> error "Unknown DID method"
    Just Web -> error "Support for did:web: is not yet implemented"
    Just Plc -> do
      req <-
        HTTP.parseRequest
          ("https://plc.directory/" <> Text.unpack rawDid)
      resp <- HTTP.httpLbs req httpManager
      case HTTP.statusCode (HTTP.responseStatus resp) of
        404 -> pure Nothing
        200 -> either fail (pure . Just) $ Aeson.eitherDecode $ HTTP.responseBody resp
        other -> fail $ "Unexpected HTTP status " <> show other
