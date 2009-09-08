module Util.YAML where

import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

data Node = Leaf String | List [Node] | Map [(String,Node)] | Null


class ToNode a where
    toNode :: a -> Node


instance ToNode Node where
    toNode x = x

instance ToNode String where
    toNode s = Leaf s

instance ToNode a => ToNode [a] where
    toNode ns = List (map toNode ns)

instance ToNode a => ToNode [(String,a)] where
    toNode ns = Map [ (x,toNode y) | (x,y) <- ns ]

instance ToNode b => ToNode (M.Map String b) where
    toNode mp = Map [(x, toNode y) | (x,y) <- M.toList mp]

instance ToNode a => ToNode (S.Set a) where
    toNode st = List $ map toNode (S.toList st)

instance ToNode a => ToNode (Maybe a) where
    toNode Nothing = Null
    toNode (Just x) = toNode x

instance (ToNode a,ToNode b) => ToNode (Either a b) where
    toNode (Left  x) = toNode x
    toNode (Right x) = toNode x

instance ToNode Bool where
    toNode True  = Leaf "true"
    toNode False = Leaf "false"

instance ToNode () where
    toNode () = Null

dumpNode :: Node -> String
dumpNode n = f False 0 n "\n" where
    f nn  i Null     = ns nn . showString "null"
    f nn  i (Leaf x) = ns nn . showString' x
    f nn i (List ns) = nl nn [ g i . showString "-" . f True (i + 1) n | n <- ns ]
    f nn i (Map  ns) = nl nn [ g i . showString x . showString ":" . f True (i + 1) y | (x,y) <- ns ]
    g i = showString $ replicate i ' '
    nl nn [] = id
    nl nn xs = (if nn then ('\n':) else id) . foldr1 (\x y -> x . showChar '\n' . y ) xs
    ns True  = showChar ' '
    ns False = id

showYAML :: ToNode a => a -> String
showYAML n = dumpNode (toNode n)

showString' x y = if all isGood x then x ++ y else '"':f x y where
    f [] y = '"':y
    f (x:xs) ys |  isQuoteGood x = x:f xs ys
                | otherwise  = '\\':x:f xs ys
    isGood x = isAlphaNum x || x `elem` "_-.@/"
    isQuoteGood x = isGood x || isSpace x || x `elem` "!@#$%^&*(){}/"

