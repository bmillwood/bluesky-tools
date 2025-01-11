{ mkDerivation, aeson, async, base, containers, dns, http-api-data
, http-client, http-types, lib, network-uri, text, transformers
}:
mkDerivation {
  pname = "bluesky-tools";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base containers dns http-api-data http-client
    http-types network-uri text transformers
  ];
  testHaskellDepends = [ base text ];
  description = "Tools for interacting with Bluesky / AT Protocol";
  license = lib.licenses.bsd3;
}
