module Data.List.Utils (
    join,
    replace,
    split,
    breakList,
    spanList,
    startswith,
    endswith
) where

import qualified Data.Array as A
import Data.List
import Data.Tuple

-- * Some list comparisons
-- All nicked from MissingH

join :: forall a. [a] -> [[a]] -> [a]
join delim l = A.concat (intersperse delim l)

replace :: forall a. (Eq a) => [a] -> [a] -> [a] -> [a]
replace old new l = join new <<< split old $ l

split :: forall a. (Eq a) => [a] -> [a] -> [[a]]
split _ [] = []
split delim str = first : second
  where
    first = fst parts
    second = case snd parts of
        [] -> []
        x  -> if x == delim
                then [] : []
                else split delim (A.drop (A.length delim) x)
    parts = breakList (startswith delim) str

breakList :: forall a. ([a] -> Boolean) -> [a] -> Tuple [a] [a]
breakList func = spanList (not <<< func)

spanList :: forall a. ([a] -> Boolean) -> [a] -> Tuple [a] [a]
spanList _ [] = Tuple [] []
spanList func list@(x:xs) =
    if func list
       then Tuple (x:(fst spanned)) (snd spanned)
       else Tuple [] list
    where spanned = spanList func xs

startswith :: forall a. (Eq a) => [a] -> [a] -> Boolean
startswith = isPrefixOf

endswith :: forall a. (Eq a) => [a] -> [a] -> Boolean
endswith = isSuffixOf

-- * Ported from Data.List

isPrefixOf              :: forall a. (Eq a) => [a] -> [a] -> Boolean
isPrefixOf [] _         =  true
isPrefixOf _  []        =  false
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

isSuffixOf              :: forall a. (Eq a) => [a] -> [a] -> Boolean
isSuffixOf x y          =  A.reverse x `isPrefixOf` A.reverse y

intersperse             :: forall a. a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll            :: forall a. a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

