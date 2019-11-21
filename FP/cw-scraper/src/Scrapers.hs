{-# LANGUAGE OverloadedStrings #-}

module Scrapers
    ( findHeader
    , findAllText
    , join2scrapers
    ) where

import           Text.HTML.Scalpel

findHeader :: Scraper String String
findHeader = text ("h1" @: ["id" @= "firstHeading"])

findAllText :: Scraper String [String]
findAllText = do
    list <- texts ("div" @: ["id" @= "mw-content-text"])
    return $ concatMap words list

join2scrapers :: Scraper String a -> Scraper String b -> Scraper String (a, b)
join2scrapers l r = do
    left <- l
    right <- r
    return (left, right)
