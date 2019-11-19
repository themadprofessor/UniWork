module Main where

import Lib

main :: IO ()
main = do 
    io <- mostfrequentwordonpage "https://en.wikipedia.org/wiki/USA"
    print io