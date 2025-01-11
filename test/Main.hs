module Main (main) where

import qualified Did
import qualified Handle

main :: IO ()
main = do
  Did.main
  Handle.main
