# pokeapi

Haskell interface to the v2 REST API of PokeAPI: https://pokeapi.co.

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
