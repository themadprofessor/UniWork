{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    ( mostfrequentwordonpage
    ) where

import Control.Exception
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.StringMap as Map
import Scrapers
import Text.HTML.Scalpel

type Header = String

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage url = do
    allText <- scrapeURL url findAllText
    skipSet <- buildSkipSet "stopwords.txt"
    return $ fmap (\content -> findMostCommon (buildMap skipSet "USA" (map normaliseWord content))) allText

normaliseWord :: String -> String
normaliseWord word =
    foldl
        (\acc c ->
             if isPunctuation c
                 then acc
                 else acc ++ [toLower c])
        []
        word

findMostCommon :: Map.StringMap Int -> String
findMostCommon counts = fst (Map.foldlWithKey isMostCommon ("", -1) counts)

isMostCommon :: (String, Int) -> String -> Int -> (String, Int)
isMostCommon prev word count =
    if (snd prev) < count
        then (word, count)
        else prev

buildMap :: Set.Set String -> Header -> [String] -> Map.StringMap Int
buildMap skipSet header allText =
    foldl
        (\acc word ->
             if isValidWord skipSet "Scotland" word
                 then updateValue acc word
                 else acc)
        Map.empty
        allText

updateValue :: Map.StringMap Int -> String -> Map.StringMap Int
updateValue acc word = Map.insert word ((Map.findWithDefault 0 word acc) + 1) acc

buildSkipSet :: String -> IO (Set.Set String)
buildSkipSet path = do
    text <- readFile path
    return (Set.fromList (lines text))

isValidWord :: Set.Set String -> Header -> String -> Bool
isValidWord _ _ [c] = False
isValidWord skipSet header word =
    (not (all isDigit word)) && (not (prefix `isPrefixOf` word)) && (Set.notMember word skipSet)
  where
    prefix = map toLower $ take 4 header