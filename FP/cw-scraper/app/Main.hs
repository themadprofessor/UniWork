{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-} 

module Main where

import Data.String (fromString)
import Text.HTML.Scalpel
import Text.Pandoc.Builder
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.RawString.QQ
import qualified Data.Text.IO as TIO
import Data.Maybe
import WikiScrapeLib

countries :: [String]
countries =
    ["Scotland", "England", "United_Kingdom", "USA", "Brazil", "France", "Germany", "Italy", "Japan", "China", "Russia"]

wikify :: String -> URL
wikify x = "https://en.wikipedia.org/wiki/" ++ x

buildPandoc :: [(String, Maybe String)] -> Pandoc
buildPandoc freq =
    setTitle "Most frequent words on a country's wikipedia page" . setAuthors ["Stuart Reilly - 2258082R"] $
    doc $
    simpleTable
        [plain "Country", plain "Most Frequent Word"]
        (map (\(x, y) -> [plain $ fromString x, plain $ fromString $ fromMaybe "not found" y]) freq)

main :: IO ()
main = do
    freqWords <- mapM mostfrequentwordonpage (wikify <$> countries)
    let results = zip countries freqWords
    output <-
        runIOorExplode $
        writeHtml5String def{writerTemplate = Just htmlTemplate } (buildPandoc results)
    TIO.writeFile "output.html" output
    
htmlTemplate :: String
htmlTemplate = [r|
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="$lang$" xml:lang="$lang$"$if(dir)$ dir="$dir$"$endif$>
<head>
  <meta charset="utf-8" />
  <meta name="generator" content="pandoc" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes" />
$for(author-meta)$
  <meta name="author" content="$author-meta$" />
$endfor$
$if(date-meta)$
  <meta name="dcterms.date" content="$date-meta$" />
$endif$
$if(keywords)$
  <meta name="keywords" content="$for(keywords)$$keywords$$sep$, $endfor$" />
$endif$
  <title>$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$</title>
  <style>
      code{white-space: pre-wrap;}
      span.smallcaps{font-variant: small-caps;}
      span.underline{text-decoration: underline;}
      div.column{display: inline-block; vertical-align: top; width: 50%;}
$if(quotes)$
      q { quotes: "“" "”" "‘" "’"; }
$endif$
  </style>
$if(highlighting-css)$
  <style>
$highlighting-css$
  </style>
$endif$
$for(css)$
  <link rel="stylesheet" href="$css$" />
$endfor$
$if(math)$
  $math$
$endif$
  <!--[if lt IE 9]>
    <script src="//cdnjs.cloudflare.com/ajax/libs/html5shiv/3.7.3/html5shiv-printshiv.min.js"></script>
  <![endif]-->
$for(header-includes)$
  $header-includes$
$endfor$
</head>
<body>
$for(include-before)$
$include-before$
$endfor$
$if(title)$
<header id="title-block-header">
<h1 class="title">$title$</h1>
$if(subtitle)$
<p class="subtitle">$subtitle$</p>
$endif$
$for(author)$
<p class="author">$author$</p>
$endfor$
$if(date)$
<p class="date">$date$</p>
$endif$
</header>
$endif$
$if(toc)$
<nav id="$idprefix$TOC" role="doc-toc">
$table-of-contents$
</nav>
$endif$
$body$
$for(include-after)$
$include-after$
$endfor$
</body>
</html>|]