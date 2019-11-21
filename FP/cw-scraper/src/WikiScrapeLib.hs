{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    ( mostfrequentwordonpage
    ) where

import qualified Control.Exception   as Exception
import qualified Control.Monad       as Monad
import           Data.Char
import           Data.Default
import           Data.List
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as Set
import qualified Data.StringMap      as Map
import           Network.HTTP.Client (HttpException)
import           Scrapers
import           Text.HTML.Scalpel

type Header = String

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage url = do
    scraped <- handleIoError $ scrapeURL url (join2scrapers findHeader findAllText)
    skipSet <- buildSkipSet "stopwords.txt"
    return
        ((\content -> maxElement (buildMap skipSet (map toLower $ fst content) (mapMaybe normaliseWord (snd content)))) =<<
         scraped)

handleIoError :: IO (Maybe a) -> IO (Maybe a)
handleIoError io = do
    may <- (Exception.try :: IO (Maybe a) -> IO (Either HttpException (Maybe a))) io
    return $
        case may of
            Left _  -> Nothing
            Right x -> x

normaliseWord :: String -> Maybe String
normaliseWord [] = Nothing
normaliseWord [_] = Nothing
normaliseWord word =
    listMaybe $
    if "'s" `isSuffixOf` word
        then []
        else foldl
                 (\acc c ->
                      if isLetter c
                          then acc ++ [toLower c]
                          else acc)
                 []
                 word

maxElement :: (Ord a, Default a) => Map.StringMap a -> Maybe String
maxElement counts =
    if common == ""
        then Nothing
        else Just common
  where
    common = fst (Map.foldlWithKey maxElementFold ("", def) counts)

maxElementFold :: Ord a => (String, a) -> String -> a -> (String, a)
maxElementFold prev word count =
    if snd prev < count
        then (word, count)
        else prev

buildMap :: Set.Set String -> Header -> [String] -> Map.StringMap Int
buildMap skipSet header =
    foldl
        (\acc word ->
             if isValidWord skipSet header word
                 then updateValue acc word
                 else acc)
        Map.empty

updateValue :: Map.StringMap Int -> String -> Map.StringMap Int
updateValue acc word = Map.insert word (Map.findWithDefault 0 word acc + 1) acc

buildSkipSet :: String -> IO (Set.Set String)
buildSkipSet path = do
    text <- readFile path
    return (Set.fromList (lines text))

isValidWord :: Set.Set String -> Header -> String -> Bool
isValidWord _ _ [c] = False
isValidWord skipSet header word = all isLetter word && not (prefix `isPrefixOf` word) && Set.notMember word skipSet
  where
    prefix = take 4 header

listMaybe :: [a] -> Maybe [a]
listMaybe [] = Nothing
listMaybe x  = Just x
