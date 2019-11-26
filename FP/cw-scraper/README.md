# cw-scraper

Stuart Reilly - 2258082

## Build

```shell script
stack build
```

## Run

```shell script
stack run
```

## Library Usage

```haskell
include WikiScrapeLib
include Text.HTML.Scapel
include Data.Maybe

-- Print the most common word on the Scotland wiki page, printing "failed to access" on failure.
main = do
    maybe_common <- mostfrequentwordonpage "https://en.wikipedia.org/wiki/Scotland"
    putStrLn $ fromMaybe maybe_common "failed to access"
```