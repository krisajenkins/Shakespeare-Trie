{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module ShakespeareTrie (someFunc, Node, readPhrases, addPhrase) where

import           Control.Monad
import           Data.Char
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Text.IO
import qualified Data.Text.IO  as TIO
import           Prelude       hiding (getLine, putStrLn, readFile)

data Node
  = Empty
  | Edge (Map Char Node)
  deriving (Eq,Show)

------------------------------------------------------------

instance Monoid Node where
  mempty = Empty
  mappend Empty y = y
  mappend x Empty = x
  mappend (Edge x) (Edge y) = Edge $ Map.unionWith mappend x y

addPhrase :: Node -> Text -> Node
addPhrase n (T.uncons -> Nothing) = n
addPhrase Empty (T.uncons -> Just (c,cs)) =
  Edge $
  Map.singleton c
                (addPhrase Empty cs)
addPhrase e@(Edge _) cs = mappend e (addPhrase Empty cs)

readPhrases :: Node -> Text -> [Text]
readPhrases Empty _ = [""]
readPhrases (Edge m) (T.uncons -> Just (c,cs)) =
  case Map.lookup c m of
    Nothing -> []
    Just subnode -> T.cons c <$> readPhrases subnode cs
readPhrases (Edge m) (T.uncons -> Nothing) =
  concatMap readSubtree (Map.assocs m)
  where readSubtree (k,v) = T.cons k <$> readPhrases v ""

------------------------------------------------------------

readShakespeare :: FilePath -> IO [Text]
readShakespeare file =
  do corpus <- readFile file
     return $ T.stripStart <$> T.lines corpus

repl :: Node -> IO ()
repl trie =
  do putStrLn "Query: "
     prefix <- getLine
     mapM_ putStrLn (take 20 (readPhrases trie prefix))
     putStrLn "-------"
     unless (T.null prefix)
            (repl trie)

someFunc :: IO ()
someFunc =
  do shakespeare <- readShakespeare "Shakespeare_Complete_Works.txt"
     let trie = foldl addPhrase Empty shakespeare
     repl trie
