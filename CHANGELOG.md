# Revision history for bluesky-tools

## 0.3.0.0

* Stop exporting Document constructor so I can continue to change it.
* getPds retrieves the PDS URI from a DID document.
* Add Handle.verifyHandle.
* Tweak Handle.resolveVerify so that failing to fetch DID document is a verification failure.

## 0.2.2.0 -- 2024-01-05

* Fix build error.
* Export unintentionally-hidden BothFailed.

## 0.2.0.0 -- 2024-01-05

* Handle type, smart constructor, and tests.
* Resolving handles to DIDs via DNS and HTTP.
* Retrieving DID documents for did:plc: DIDs, and verifying handles against them.
