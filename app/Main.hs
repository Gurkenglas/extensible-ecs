module Main where
import Control.Monad.State
import Lib

import SystemFoo
import SystemBar
import SystemBaz

main :: IO ()
main = do
    void . flip runStateT newWorld $ do
        initSystemFoo
        initSystemBar
        initSystemBaz

        replicateM_ 10 $ do
            tickSystemFoo
            tickSystemBar
            tickSystemBaz
