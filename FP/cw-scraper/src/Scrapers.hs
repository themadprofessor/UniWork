{-# LANGUAGE OverloadedStrings #-}
module Scrapers (findHeader, findAllText) where

import Text.HTML.Scalpel

findHeader :: Scraper String String
findHeader = text ("h1" @: ["id" @= "firstHeading"])

findAllText :: Scraper String [String]
findAllText = do
    list <- texts (("div" @: ["id" @= "mw-content-text"])//textSelector)
    return $ (concatMap words list)