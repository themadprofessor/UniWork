{-# LANGUAGE OverloadedStrings #-}

module Scrapers
    ( findHeader
    , findAllText
    , join2scrapers
    ) where

import           Text.HTML.Scalpel
import qualified Data.Text as T
import Control.Applicative

-- Create a scraper to find the header of a wikipedia page.
findHeader :: Scraper String T.Text
findHeader = T.pack <$> text ("h1" @: ["id" @= "firstHeading"])

-- Create a scraper which finds all words in the body of a wikipedia page.
-- The body is defined as a div with the id "mw-content-text".
findAllText :: Scraper String [T.Text]
findAllText = do
    list <- chroot ("div" @: ["id" @= "mw-content-text"]) $ texts anySelector
    return $ concatMap (T.words . T.pack) list

-- Convert two scrapers into a single scraper which returns a tuple of the results of the given scrapers.
join2scrapers :: Scraper String a -> Scraper String b -> Scraper String (a, b)
join2scrapers l r = do
    left <- l
    right <- r
    return (left, right)
