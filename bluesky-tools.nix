{ mkDerivation, aeson, async, base, dns, http-api-data, http-client
, http-types, lib, text, transformers
}:
mkDerivation {
  pname = "bluesky-tools";
  version = "0.2.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base dns http-api-data http-client http-types text
    transformers
  ];
  testHaskellDepends = [ base text ];
  description = "Tools for interacting with Bluesky / AT Protocol";
  license = lib.licenses.bsd3;
}
