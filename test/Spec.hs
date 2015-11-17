import Test.Hspec
import ShakespeareTrie
import           Data.Text     (Text)
import qualified Data.Text     as T

strings = ["bar", "BaR", "bor", "tor"]
texts = map T.pack strings

trie1 :: Node
trie1 = foldl addPhrase mempty texts

main :: IO ()
main = hspec $ do
    describe "integration" $ do
        describe "simple case sensitive trie" $ do
            it "returns nothing if not found" $ do
                readPhrases trie1 (T.pack("q")) `shouldMatchList` mempty
            it "returns element starting with BaR" $ do
                readPhrases trie1 (T.pack("BaR")) `shouldMatchList` [T.pack("BaR")]
            it "returns elementi starting with b" $ do
                readPhrases trie1 (T.pack("b")) `shouldMatchList` [T.pack("bar"), T.pack("bor")]
            it "returns all elements" $ do
                readPhrases trie1 (T.pack("")) `shouldMatchList` texts
--      describe "simple case insensitive trie" $ do
--          it "returns nothing if not found" $ do
--              readPhrasesCaseInsens trie1 (T.pack("q")) `shouldMatchList` mempty
--          it "returns element starting with BaR" $ do
--              readPhrasesCaseInsens trie1 (T.pack("BaR")) `shouldMatchList` [T.pack("BaR")]
--          it "returns element starting with b" $ do
--              readPhrasesCaseInsens trie1 (T.pack("b")) `shouldMatchList` [T.pack("bar"), T.pack("bor"), T.pack("BaR")]
--          it "returns all elements" $ do
--              readPhrasesCaseInsens trie1 (T.pack("")) `shouldMatchList` texts
    describe "unit" $ do
        describe "keyVariants" $ do
            it "returns upper- and lowercase for letters" $ do
                keyVariants 'c' `shouldMatchList` ['c', 'C']
            it "return single char for non letters" $ do
                keyVariants ',' `shouldMatchList` [',']
