{-# LANGUAGE OverloadedStrings #-}

module Scrapers
    ( findHeader
    , findAllText
    , join2scrapers
    ) where

import           Text.HTML.Scalpel
import qualified Data.Text as T
import Control.Applicative

findHeader :: Scraper String T.Text
findHeader = T.pack <$> text ("h1" @: ["id" @= "firstHeading"])

findAllText :: Scraper String [T.Text]
findAllText = do
    list <- chroot ("div" @: ["id" @= "mw-content-text"]) $ texts anySelector
    return $ concatMap (T.words . T.pack) list

join2scrapers :: Scraper String a -> Scraper String b -> Scraper String (a, b)
join2scrapers l r = do
    left <- l
    right <- r
    return (left, right)
