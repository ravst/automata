module Automata.NFASpec where

import Automata.NFA
import Automata.Tools
import Test.Hspec
import Test.QuickCheck

a :: Char
a = 'a'

b :: Char
b = 'b'

alphabet :: [Char]
alphabet = [a, b]

oddB :: NFA Char Int
oddB = makeAutomaton [1] [(a, 1, 1), (a, 2, 2), (b, 1, 2), (b, 2, 1)] [2]

containsAba :: NFA Char Int
containsAba = makeAutomaton
  [1] [(a, 1, 1), (b, 1, 1), (a, 1, 2), (b, 2, 3), (a, 3, 4), (a, 4, 4), (b, 4, 4)] [4]

oddAOrOddB :: NFA Char Int
oddAOrOddB = makeAutomaton
  [1,3] [('a', 1, 2), ('a', 2, 1), ('b', 1, 1), ('b', 2, 2), ('a', 3, 3), ('a', 4, 4), ('b', 3, 4), ('b', 4, 3)] [2,4]

abStrings :: Gen String
abStrings = listOf $ elements [a, b]

disconnectedAutomaton = makeAutomaton
  [3] [(a, 1, 1), (b, 2, 2), (a, 1, 2), (b, 2, 1), (a, 3, 4), (b, 4, 4)] [2]

type CharWithEps = Either () Char

ea :: CharWithEps
ea = Right 'a'

eb :: CharWithEps
eb = Right 'b'

eps :: CharWithEps
eps = Left ()

epsAutomaton :: NFA CharWithEps Int
epsAutomaton = makeAutomaton [1] [(ea, 1, 2), (ea, 2, 6), (eps, 2, 3), (eps, 3, 4), (eps, 4, 2), (eb, 4, 5), (eps, 1, 7), (eb, 7, 8)] [5, 6, 8]

test :: Spec
test = do
  context "when checking if an automation accepts a word" $ do
    it "should accepts words in the language" $ do
      (accepts oddB "abbab") `shouldBe` True
      (accepts oddB "aba") `shouldBe` True
      (accepts containsAba "aaaabbbabaaabb") `shouldBe` True
      (accepts containsAba "aba") `shouldBe` True
    it "should not accept words outside of the language" $ do
      (accepts oddB "babbab") `shouldBe` False
      (accepts oddB "ababbb") `shouldBe` False
      (accepts containsAba "aaaabbbabbba") `shouldBe` False
      (accepts containsAba "abba") `shouldBe` False
  context "when checking if an automation fast--accepts a word" $ do
    it "should accepts words in the language" $ do
      (acceptsFast oddB "abbab") `shouldBe` True
      (acceptsFast oddB "aba") `shouldBe` True
      (acceptsFast containsAba "aaaabbbabaaabb") `shouldBe` True
      (acceptsFast containsAba "aba") `shouldBe` True
    it "should not accept words outside of the language" $ do
      (acceptsFast oddB "babbab") `shouldBe` False
      (acceptsFast oddB "ababbb") `shouldBe` False
      (acceptsFast containsAba "aaaabbbabbba") `shouldBe` False
      (acceptsFast containsAba "abba") `shouldBe` False

  context "union of two languages" $ do
    let u = unionAutomaton oddB containsAba
    it "contains a word iff it belongs at least to one of the languages" $ forAll abStrings $
      \x -> accepts u x == ((accepts oddB x) || (accepts containsAba x))
  context "product of lanugages" $ do
    let p = intersectionAutomaton oddB containsAba
    it "contains a word iff it belongs to both of the languages" $ forAll abStrings $
      \x -> accepts p x == ((accepts oddB x) && (accepts containsAba x))
  context "when looking for reachable nodes" $ do
    it "all nodes are returned for conneted automata" $ do
      reachableStates alphabet containsAba `shouldMatchList` [1, 2, 3, 4]
      reachableStates alphabet (normaliseAutomaton alphabet oddAOrOddB) `shouldMatchList` [0,1,2,3]
    it "some nodes are retured for disconected automata" $
      reachableStates alphabet disconnectedAutomaton `shouldMatchList` [3, 4]
  context "normalised automaton" $ do
    let n = normaliseAutomaton alphabet containsAba
    it "recognises the same language as the original automaton"$ forAll abStrings $
      \x -> accepts n x == accepts containsAba x
    it "recognises the same language as the original automaton" $
      normaliseAutomaton alphabet oddAOrOddB `automataMatchOnAB` oddAOrOddB
  context "when testing emptiness" $ do
    it "yields correct result when given a non-empty language" $
      isEmpty alphabet containsAba `shouldBe` False
    it "yields correct result when given an empty language" $
      isEmpty alphabet disconnectedAutomaton `shouldBe` True
  context "when filtering letters" $ do
    it "accepts the same words with the new alphabet" $
      filterAlphabet epsAutomaton `shouldAccept` "aa"
    it "does not use 'epsilon'-edges" $ do
      filterAlphabet epsAutomaton `shouldNotAccept` "ab"
  context "when projecting letters" $ do
    it "accpets the same words with the new alphabet" $
      alphabetProjection [()] epsAutomaton `shouldAccept` "aa"
    it "uses 'epsilon-edges'" $ do
      alphabetProjection [()] epsAutomaton `shouldAccept` "ab"
    it "uses 'epsilon-edges' to decide on initial states" $ do
      alphabetProjection [()] epsAutomaton `shouldAccept` "b"
    it "does not accept all the words" $ do
      alphabetProjection [()] epsAutomaton `shouldNotAccept` "bb"
  context "when making one accepting and one initial state" $ do
    it "accepts the same language as the original automaton" $ do
      let aut = unionAutomaton oddB containsAba
      let oneStateAut = alphabetProjection [()] $ makeOneInitialAndOneAcceptingState aut
      automataMatchOnAB aut oneStateAut
  context "when applying an iso to an automaton" $ do
    it "accepts the same words" $ do
      automataMatchOnAB containsAba $ stateIsomorphism (+5) (\x -> x -5) containsAba
  context "when serialising and deserialising an automaton" $ do
    let transitions = getTransitions [a,b] containsAba
    let accepting = getAcceptingStates containsAba
    let initial = initialStates containsAba
    it "accepts the same words as the original automaton" $ do
      containsAba `automataMatchOnAB` makeAutomaton initial transitions accepting

spec :: Spec
spec = test
