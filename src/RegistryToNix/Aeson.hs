
module RegistryToNix.Aeson where

import Data.Char
import qualified Data.List as L


toSnake :: String -> String
toSnake = fmap toLower . L.concat . underscores . splitR isUpper
  where
    underscores :: [String] -> [String]
    underscores [] = []
    underscores (h:t) = h : fmap ('_':) t

    splitR :: (Char -> Bool) -> String -> [String]
    splitR _ [] = []
    splitR p s =
      let
        go :: Char -> String -> [String]
        go m s' = case L.break p s' of
          (b', [])     -> [ m:b' ]
          (b', x:xs) -> ( m:b' ) : go x xs
      in case L.break p s of
        (b,  [])    -> [ b ]
        ([], h:t) -> go h t
        (b,  h:t) -> b : go h t
