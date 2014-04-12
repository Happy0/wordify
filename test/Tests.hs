-- Following the example of: https://github.com/bos/text/blob/master/tests/Tests.hs
module Main
    ( main
    ) where

import Test.Framework.Runners.Console

import qualified Tests.Properties as Properties
import qualified Tests.Regressions as Regressions

main :: IO ()
main = defaultMain [Properties.tests, Regressions.tests]