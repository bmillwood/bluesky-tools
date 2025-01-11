module Bluesky.Did
  ( Did, rawDid, makeDid, DidError(..)
  , Document(alsoKnownAs), getPds
  , getDocument
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (First(First, getFirst))
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import GHC.Stack

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.URI as URI

-- | https://atproto.com/specs/did
--
-- A DID is a Decentralized Identifier. They're codified by various W3C
-- standards. This type only aims to capture how they are used in atproto.
newtype Did = Did { rawDid :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON)

data DidError
  = NoDidPrefix
  | NoMethodSeparator
    -- ^ after "did:", there must be another colon to delimit method from
    -- identifier
  | BadMethod
    -- ^ method must match the regex @[a-z]+@; this does not check if the method
    -- is supported by atproto
  | BadIdentifierCharacters
    -- ^ identifier section must match @[a-zA-Z0-9._:%-]@
    -- (nb. that general DID URIs may also have query and fragment components,
    -- i.e. @?@ and @#@ characters, but atproto DID identifiers may not)
  | EndsWithColon
  | BadPercentEncoding
    -- ^ the spec says implementations don't need to validate percent encoding,
    -- but we validate that every % is followed by two hex digits
  deriving stock (Show, Eq, Ord)

makeDid :: Text -> Either DidError Did
makeDid raw
  | not ("did:" `Text.isPrefixOf` raw) = Left NoDidPrefix
  | Text.null method
    || Text.any (not . isMethodChar) method = Left BadMethod
  | Text.any (not . isIdentifierChar) colonBody = Left BadIdentifierCharacters
  | Text.takeEnd 1 raw == ":" = Left EndsWithColon
  | any (not . Text.all Char.isHexDigit) percentEncodes = Left BadPercentEncoding
  | otherwise = Right (Did raw)
  where
    afterPrefix = Text.drop 4 raw
    (method, colonBody) = Text.breakOn ":" afterPrefix
    isMethodChar c = Char.isAscii c && Char.isLower c
    isIdentifierChar c =
      Char.isAscii c
      && (Char.isAlphaNum c || elem c ("._:%-" :: [Char]))
    percentEncodes = map (Text.take 2) . drop 1 $ Text.splitOn "%" colonBody

instance Aeson.FromJSON Did where
  parseJSON = Aeson.withText "Bluesky.Did.Did" $ either (fail . show) pure . makeDid

-- | The DID methods supported by atproto. Note that DIDs are used outside of
-- atproto and there are many more methods in those contexts, but we don't
-- support them here.
data Method
  = Web -- ^ https://w3c-ccg.github.io/did-method-web/
  | Plc -- ^ https://github.com/did-method-plc/did-method-plc
  deriving stock (Eq, Ord, Show)

getMethod :: Did -> Maybe Method
getMethod Did{ rawDid }
  | "did:web:" `Text.isPrefixOf` rawDid = Just Web
  | "did:plc:" `Text.isPrefixOf` rawDid = Just Plc
  | otherwise = Nothing

data Service = Service
  { serviceId :: Text
  , serviceType :: Text
  , serviceEndpoint :: URI.URI
  } deriving stock (Eq, Ord, Show, Generic)

instance Aeson.FromJSON Service where
  parseJSON = Aeson.withObject "Service" $ \o -> do
    serviceId <- o .: "id"
    serviceType <- o .: "type"
    serviceEndpointString <- o .: "serviceEndpoint" -- [sic]
    serviceEndpoint <-
      maybe
        (fail $ "Couldn't parse serviceEndpoint URI: " <> show serviceEndpointString)
        pure
        $ URI.parseURI serviceEndpointString
    pure Service{ serviceId, serviceType, serviceEndpoint }

-- | Fields that the library currently doesn't understand are ignored.
data Document = Document
  { documentId :: Did
  , alsoKnownAs :: [Text]
  , service :: [Service]
  } deriving stock (Eq, Ord, Show, Generic)

getPds :: Document -> Maybe URI.URI
getPds Document{ service } =
  getFirst $ foldMap (First . get) service
  where
    get Service{ serviceId, serviceType, serviceEndpoint }
      | "#atproto_pds" `Text.isSuffixOf` serviceId
        && serviceType == "AtprotoPersonalDataServer"
        = Just serviceEndpoint
      | otherwise = Nothing

genericParseJSONMapFields
  :: (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a))
  => [(String, String)] -> Aeson.Value -> Aeson.Parser a
genericParseJSONMapFields fields =
  Aeson.genericParseJSON
    Aeson.defaultOptions{ Aeson.fieldLabelModifier = mapFields }
  where
    mapFields field = fromMaybe field (Map.lookup field fieldsMap)
    fieldsMap = Map.fromList fields

instance Aeson.FromJSON Document where
  parseJSON =
    genericParseJSONMapFields
      [("documentId", "id")]

-- | This is currently only implemented for did:plc: DIDs.
getDocument :: HasCallStack => HTTP.Manager -> Did -> IO (Maybe Document)
getDocument httpManager did@(Did rawDid) =
  case getMethod did of
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
