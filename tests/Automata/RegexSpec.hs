{-# LANGUAGE QuasiQuotes #-}
module Automata.RegexSpec where

import Debug.Trace

import Test.Hspec
import Text.Parsec
import Data.Void
import Control.Monad
import Automata.Regex.Parser
import Automata.Regex.Data
import Automata.Regex.Compiler
import Automata.Regex.QQ
import Automata.Tools
import Automata.NFA


regexString :: String
regexString = "a | a(\\\\#(Automaton)(a|b)) | \\#\\(\\)"

regexAns :: Regex Char String
regexAns = (Union (Concat (Letter 'a') (Letter ' ')) (Union (Concat (Letter ' ') (Concat (Letter 'a') (Concat (Concat (Letter '\\') (Concat (Feature "Automaton") (Union (Letter 'a') (Letter 'b')))) (Letter ' ')))) (Concat (Letter ' ') (Concat (Letter '#') (Concat (Letter '(') (Letter ')'))))))

cleanRegex :: Regex Char Void
cleanRegex = Iterate $ Union (Concat (Letter 'a') (Letter 'b')) (Concat (Letter 'b') (Letter 'a'))

oddAOrOddB :: NFA Char Int
oddAOrOddB = makeAutomaton [1,3] [('a', 1, 2), ('a', 2, 1), ('b', 1, 1), ('b', 2, 2), ('a', 3, 3), ('a', 4, 4), ('b', 3, 4), ('b', 4, 3)] [2,4]

automatonRegex :: Regex Char (NFA Char Int)
automatonRegex = Iterate $ Concat (Letter '[') (Concat (Feature oddAOrOddB) (Letter ']'))

quotedRegex :: Regex Char (NFA Char Int)
quotedRegex = [regex|([#(oddAOrOddB)])*|]

spec :: Spec
spec = do
  context "when parsing regexps" $ do
    it "parses a correct regex correctely" $ do
      parse parseRegex "tttt" regexString `shouldBe` Right regexAns
      parse parseRegex "tttt" "a*" `shouldBe` (Right $ Iterate $ Letter 'a')
  context "when compilng regex without features" $ do
    let toAccept = ["ab", "ba", "abba", ""]
    let toReject = ["aba", "aa", "a", "bb", "bba"]
    let automaton = compileRegex ['a', 'b'] cleanRegex
    it "accepts good words" $ forM_ toAccept (\ok -> automaton `shouldAccept` ok)
    it "rejects bad words" $ forM_ toReject (\no -> automaton `shouldNotAccept` no)
  context "when compiling regex with automata features" $ do
    let automaton = compileRegex ['a', 'b', '[', ']'] automatonRegex
    let toAccept = ["[aab][bbbbaaa][aabbb]", "", "[a][aaa]"]
    let toReject = ["abb", "[aaaabbbb]", "[aabb][aa]"]
    it "accepts good words" $ forM_ toAccept (\ok -> automaton `shouldAccept` ok)
    it "rejects bad words" $ forM_ toReject (\no -> automaton `shouldNotAccept` no)
  context "when compiling quasiquoted regex" $ do
    let automaton = compileRegex ['a', 'b', '[', ']'] quotedRegex
    traceShowM quotedRegex
    let toAccept = ["[aab][bbbbaaa][aabbb]", "", "[a][aaa]"]
    let toReject = ["abb", "[aaaabbbb]", "[aabb][aa]"]
    it "accepts good words" $ forM_ toAccept (\ok -> automaton `shouldAccept` ok)
    it "rejects bad words" $ forM_ toReject (\no -> automaton `shouldNotAccept` no)


