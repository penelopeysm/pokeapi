# pokeapi

Haskell interface to the v2 REST API of PokeAPI: https://pokeapi.co.

## Example usage

```haskell
import Pokeapi
import qualified Data.Text.IO as T

main = do
  toge <- get "togekiss"
  let entry = filter (\ft -> (name $ ftLanguage ft) == "en"
                             && (name <$> ftVersion ft) == Just "soulsilver")
                     (psFlavorTextEntries toge)
  case entry of
       [e] -> T.putStrLn (ftFlavorText e)
       _   -> pure ()
```

> *As everyone knows, it visits peaceful*  
> *regions, bringing them gifts of kindness*  
> *and sweet blessings.*

## Switching the base URL

Set the `POKEAPI_BASE_URL` environment variable to wherever you are hosting the data on.
For example, when developing locally (using the Makefile in the [original repository](https://github.com/pokeapi/pokeapi)), you can set this to `http://localhost:8000`.
(Note that the `http://` prefix is mandatory.)

## Caching

PokeAPI generally asks that you locally cache results where possible.

By default, the results of HTTP requests are cached inside `~/.pokeapi_cache`.

You can specify the cache directory using the `POKEAPI_CACHE_LOCATION` environment variable.

If you do not want to use caching at all, set the `POKEAPI_NO_CACHE` environment variable to any non-empty value.
