module Lib
    ( cli
    ) where

import qualified CLI.Main as CM

cli :: IO ()
cli = CM.main
