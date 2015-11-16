module Lib
    ( someFunc
    ) where

import           Data.Char
import           Data.Map  (Map)
import qualified Data.Map  as Map

data Node
  = Empty
  | Edge (Map Char Node)
  deriving (Eq,Show)

------------------------------------------------------------

addPhrase :: String -> Node -> Node
addPhrase [] n = n
addPhrase (c:cs) Empty =
  Edge (Map.singleton c
                      (addPhrase cs Empty))
addPhrase (c:cs) (Edge m) =
  Edge $
  Map.insert
    c
    (addPhrase cs
               (case Map.lookup c m of
                  Nothing -> Empty
                  Just subnode -> subnode))
    m

readSubtree :: (Char,Node) -> [String]
readSubtree (k,v) = fmap (k:) (readPhrases v [])

readPhrases :: Node -> String -> [String]
readPhrases Empty _ = [""]
readPhrases (Edge m) [] = concat $ fmap readSubtree (Map.assocs m)
readPhrases (Edge m) (c:cs) =
  case Map.lookup c m of
    Just subnode -> fmap (c :) (readPhrases subnode cs)
    Nothing -> []

------------------------------------------------------------

a :: Node
a = addPhrase "bar" $ addPhrase "baz" $ addPhrase "foo" Empty

b :: [String]
b = readPhrases a "q"

------------------------------------------------------------

readShakespeare :: FilePath -> IO [String]
readShakespeare file =
  do corpus <- readFile file
     return $ fmap (dropWhile isSpace) $ lines corpus

someFunc :: IO ()
someFunc =
  do shakespeare <- readShakespeare "Shakespeare_Complete_Works.txt"
     let trie =
           foldl (\acc line -> addPhrase line acc)
                 Empty
                 shakespeare
     mapM_ print (take 20 (readPhrases trie "CLOWN"))
