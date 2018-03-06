module Main
  ( main
  ) where

import           Blockchain (runBlockchain)

-- |'main' function.
main :: IO ()
main = runBlockchain Nothing
