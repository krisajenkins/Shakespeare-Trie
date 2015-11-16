module Lib
    ( someFunc
    ) where

import           Control.Monad
import           Data.Char
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe

data Node
  = Empty
  | Edge (Map Char Node)
  deriving (Eq,Show)

------------------------------------------------------------

addPhrase :: Node -> String -> Node
addPhrase n [] = n
addPhrase Empty (c:cs) =
  Edge (Map.singleton c
                      (addPhrase Empty cs))
addPhrase (Edge m) (c:cs) =
  Edge $ Map.alter (\mk -> Just $ addPhrase (fromMaybe Empty mk) cs) c m

readSubtree :: (Char,Node) -> [String]
readSubtree (k,v) = (k :) <$> readPhrases v []

readPhrases :: Node -> String -> [String]
readPhrases Empty _ = [""]
readPhrases (Edge m) [] = concatMap readSubtree (Map.assocs m)
readPhrases (Edge m) (c:cs) =
  case Map.lookup c m of
    Nothing -> []
    Just subnode -> (c :) <$> readPhrases subnode cs

------------------------------------------------------------

a :: Node
a = foldl addPhrase Empty ["bar", "baz", "foo"]

b :: [String]
b = readPhrases a "q"

------------------------------------------------------------

readShakespeare :: FilePath -> IO [String]
readShakespeare file =
  do corpus <- readFile file
     return $ dropWhile isSpace <$> lines corpus

repl :: Node -> IO ()
repl trie =
  do putStrLn "Query: "
     prefix <- getLine
     mapM_ putStrLn (take 20 (readPhrases trie prefix))
     putStrLn "-------"
     unless (null prefix)
            (repl trie)

someFunc :: IO ()
someFunc =
  do shakespeare <- readShakespeare "Shakespeare_Complete_Works.txt"
     let trie = foldl addPhrase Empty shakespeare
     repl trie
