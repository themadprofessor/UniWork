{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    ( mostfrequentwordonpage
    ) where

import qualified Control.Exception   as Exception
import qualified Control.Monad       as Monad
import           Data.Char
import           Data.Maybe          (mapMaybe, fromMaybe, fromJust)
import qualified Data.HashSet        as Set
import qualified Data.HashMap.Strict as Map
import           Network.HTTP.Client (HttpException)
import           Scrapers
import           Text.HTML.Scalpel
import qualified Data.Text           as T
import           Control.Applicative
import           Data.Ord

type Header = T.Text
type Counts = Map.HashMap T.Text Int

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage url = do
    scraped <- handleIoError $ scrapeURL url (join2scrapers findHeader findAllText)
    skipSet <- buildSkipSet "stopwords.txt"
    return
        ((\content ->
              T.unpack <$>
              maxElement (buildMap skipSet (T.toLower $ fst content) (mapMaybe normaliseWord (snd content)))) =<<
         scraped)

handleIoError :: IO (Maybe a) -> IO (Maybe a)
handleIoError io = do
    may <- (Exception.try :: IO (Maybe a) -> IO (Either HttpException (Maybe a))) io
    return $
        case may of
            Left _  -> Nothing
            Right x -> x

normaliseWord :: T.Text -> Maybe T.Text
normaliseWord word =
    textMaybe $
    if "'s" `T.isSuffixOf` word
        then norm $ T.dropEnd 2 word
        else norm word
  where
    norm =
        T.toLower .
        T.pack .
        T.foldl'
            (\acc c ->
                 if isLetter c
                     then acc ++ [c]
                     else acc)
            []

maxElement :: Counts -> Maybe T.Text
maxElement counts =
    if T.null common
        then Nothing
        else Just common
  where
    common = fst (Map.foldlWithKey' maxElementFold ("", 0) counts)

maxElementFold :: Ord a => (T.Text, a) -> T.Text -> a -> (T.Text, a)
maxElementFold prev word count =
    if snd prev < count
        then (word, count)
        else prev

buildMap :: Set.HashSet T.Text -> Header -> [T.Text] -> Counts
buildMap skipSet header =
    foldl
        (\acc word ->
             if isValidWord skipSet header word
                 then updateValue acc word
                 else acc)
        Map.empty

updateValue :: Counts -> T.Text -> Counts
updateValue map word = Map.alter (Just . (+ 1) . fromMaybe 0) word map

buildSkipSet :: String -> IO (Set.HashSet T.Text)
buildSkipSet path = do
    text <- readFile path
    return (Set.fromList (T.lines $ T.pack text))

isValidWord :: Set.HashSet T.Text -> Header -> T.Text -> Bool
isValidWord skipSet header word = T.compareLength word 1 == GT && T.all isLetter word && not (prefix `T.isPrefixOf` word) && not (word `Set.member` skipSet)
  where
    prefix = T.take 4 header

textMaybe :: T.Text -> Maybe T.Text
textMaybe t = if T.null t then Nothing else Just t
