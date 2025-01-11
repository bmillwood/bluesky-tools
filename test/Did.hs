module Did where

import Data.Text (Text)

import Bluesky.Did

testAccept :: Text -> IO ()
testAccept t = case makeDid t of
  Left err -> error $ "Did not accept " <> show t <> ": " <> show err
  Right _ -> pure ()

testReject :: DidError -> Text -> IO ()
testReject expectedErr t = case makeDid t of
  Left actualErr
    | expectedErr == actualErr -> pure ()
    | otherwise -> error $ "Wrong error for " <> show t <> ": " <> show actualErr
  Right _ -> error $ "Incorrectly accepted " <> show t

main :: IO ()
main = do
  -- examples from the docs
  mapM_ testAccept
    [ -- valid and usable
      "did:plc:z72i7hdynmk6r22z27h6tvur"
    , "did:web:blueskyweb.xyz"
    , -- syntactically valid but invalid method
      "did:method:val:two"
    , "did:m:v"
    , "did:method::::val"
    , "did:method:-:_:."
    , "did:key:zQ3shZc2QzApp2oymGvQbzP8eKheVshBHbU4ZYjeXqwSKEn6N"
    ]
  -- invalid syntax
  testReject BadMethod "did:METHOD:val"
  testReject BadMethod "did:m123:val"
  testReject NoDidPrefix "DID:method:val"
  testReject EndsWithColon "did:method:"
  testReject BadIdentifierCharacters "did:method:val/two"
  testReject BadIdentifierCharacters "did:method:val?two"
  testReject BadIdentifierCharacters "did:method:val#two"

  -- my own examples
  testAccept "did:method:A:b"
  -- percent encoding verification
  testAccept "did:plc:%ff"
  testAccept "did:plc:%F0"
  testReject BadPercentEncoding "did:plc:%FG"
