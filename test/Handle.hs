module Handle where

import Data.Text (Text)

import Bluesky.Handle

testAccept :: Text -> IO ()
testAccept t = case makeHandle t of
  Left err -> error $ "Did not accept " <> show t <> ": " <> show err
  Right _ -> pure ()

testReject :: HandleError -> Text -> IO ()
testReject expectedErr t = case makeHandle t of
  Left actualErr
    | expectedErr == actualErr -> pure ()
    | otherwise -> error $ "Wrong error for " <> show t <> ": " <> show actualErr
  Right _ -> error $ "Incorrectly accepted " <> show t

main :: IO ()
main = do
  -- examples from the docs
  mapM_ testAccept
    [ "jay.bsky.social"
    , "8.cn"
    , "name.t--t"
    , "XX.LCS.MIT.EDU"
    , "a.co"
    , "xn--notarealidn.com"
    , "xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s"
    , "xn--ls8h.test"
    , "example.t"
    ]
  testReject BadCharacters "jo@hn.test"
  testReject BadCharacters "💩.test"
  testReject EmptySegment "john..test"
  testReject SegmentEndsWithHyphen "xn--bcher-.tld"
  testReject LastSegmentStartsWithNumber "john.0"
  testReject LastSegmentStartsWithNumber "cn.8"
  testReject BadCharacters "www.masełkowski.pl.com"
  testReject OnlyOneSegment "org"
  testReject EmptySegment "name.org."
