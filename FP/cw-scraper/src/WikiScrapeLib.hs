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

-- Find the most common word on the wikipedia page at the given URL.
-- Words are ignored if:
--   - Has less then 2 characters
--   - Is not exclusively letters
--   - Is found in the stopwords.txt file
--   - Has the same first 4 letters as the header of the wikipedia page
mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage url = do
    -- scraped is the tuple (header_of_page, list_of_words)
    scraped <- handleIoError $ scrapeURL url (join2scrapers findHeader findAllText)
    skipSet <- buildSkipSet "stopwords.txt"
    return
        ((\content ->
              T.unpack <$>
              maxElement (buildMap skipSet (T.toLower $ fst content) (mapMaybe normaliseWord (snd content)))) =<<
         scraped)

-- Handle a HTTPException by returning IO Nothing on exception
handleIoError :: IO (Maybe a) -> IO (Maybe a)
handleIoError io = do
    may <- (Exception.try :: IO (Maybe a) -> IO (Either HttpException (Maybe a))) io
    return $
        case may of
            Left _  -> Nothing
            Right x -> x

-- Normalise the given word, returning Nothing if the normalised word is empty.
-- This will remove a trailing "'s", all non-letter characters, and set every character to lower case.
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

-- Find the key with the largest value.
-- Used to find the word with the highest frequency.
maxElement :: Counts -> Maybe T.Text
maxElement counts =
    if T.null common
        then Nothing
        else Just common
  where
    common = fst (Map.foldlWithKey' maxElementFold ("", 0) counts)

-- The function used in maxElement's foldl
-- Returns the first argument if the second element of the first argument is larger than the third arguemnt, otherwise
-- returns the second and first argument as a tuple.
maxElementFold :: Ord a => (b, a) -> b -> a -> (b, a)
maxElementFold prev word count =
    if snd prev < count
        then (word, count)
        else prev

-- Constructs the map of words to frequencies.
-- The first argument is the set of words to ignore.
-- The second argument is the header of the page.
-- The third argument is the list of words to build the map from.
buildMap :: Set.HashSet T.Text -> Header -> [T.Text] -> Counts
buildMap skipSet header =
    foldl
        (\acc word ->
             if isValidWord skipSet header word
                 then updateValue acc word
                 else acc)
        Map.empty

-- Updates the value in the given map with the given key.
-- If the word doesn't exist in the map, its given a value of one.
-- If the word exists in the map, its value is incremented.
updateValue :: Counts -> T.Text -> Counts
updateValue map word = Map.alter (Just . (+ 1) . fromMaybe 0) word map

-- Build a set of words to skip from the words in the file at the given path.
-- This function assumes the file has only one word per line, and no blank lines.
buildSkipSet :: String -> IO (Set.HashSet T.Text)
buildSkipSet path = do
    text <- readFile path
    return (Set.fromList (T.lines $ T.pack text))

-- Determine if the given word should be included.
-- A word is valid if:
--   - It has at least 2 characters
--   - It contains only letters
--   - The first four letters are not the same as the first four letters of the given header
--   - The word is not in the given set of skip words
isValidWord :: Set.HashSet T.Text -> Header -> T.Text -> Bool
isValidWord skipSet header word = T.compareLength word 1 == GT && T.all isLetter word && not (prefix `T.isPrefixOf` word) && not (word `Set.member` skipSet)
  where
    prefix = T.take 4 header

-- Handle empty Texts.
-- If the given text is empty, return Nothing, otherwise return Just text.
textMaybe :: T.Text -> Maybe T.Text
textMaybe t = if T.null t then Nothing else Just t
