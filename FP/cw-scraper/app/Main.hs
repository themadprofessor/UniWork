module Main where

import           Text.HTML.Scalpel
import           WikiScrapeLib

countries :: [String]
countries =
    ["Scotland", "England", "United_Kingdom", "USA", "Brazil", "France", "Germany", "Italy", "Japan", "China", "Russia"]

wikify :: String -> URL
wikify x = "https://en.wikipedia.org/wiki/" ++ x



main :: IO ()
main = do
    freqWords <- mapM mostfrequentwordonpage (wikify <$> countries)
    let results = zip countries freqWords
    mapM_ (\x -> putStrLn $ fst x ++ ": " ++ (show . snd) x) results
