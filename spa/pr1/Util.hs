module Util where

import Data.Maybe (fromJust, isJust)

-- associative array update
aeUpd :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
aeUpd a b = aeUpdF a (\_ -> b)
-- associative array update function
aeUpdF :: (Eq a) => a -> (b -> b) -> [(a, b)] -> [(a, b)]
aeUpdF a f ae = let b = fromJust $ lookup a ae
                    fae = filter (\(ax, _) -> ax /= a) ae
                in  (a, f b) : fae

fromJustX str m = if isJust m then fromJust m else error(str)

jLookup :: (Eq a) => a -> [(a, b)] -> b
jLookup a ae = if isJust lu
               then fromJust lu
               else error "jLookup"
  where
    lu = lookup a ae

sndMap f = map (\(a, b) -> (a, f b))
