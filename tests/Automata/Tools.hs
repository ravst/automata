module Automata.Tools where

import Automata.NFA
import Test.QuickCheck
import Test.Hspec

automataMatchOn :: (Show a) => [a] -> NFA a s1 -> NFA a s2 -> Property
automataMatchOn alphabet a1 a2 = forAll (listOf $ elements alphabet) $
    \w -> accepts a1 w == accepts a2 w

automataMatchOnAB :: NFA Char s1 -> NFA Char s2 -> Property
automataMatchOnAB a1 a2 = automataMatchOn ['a', 'b'] a1 a2

shouldAccept :: (Show a, Show s1) => NFA a s1 -> [a] -> Expectation
shouldAccept automaton w = w `shouldSatisfy` (accepts automaton)

shouldNotAccept :: (Show a, Show s1) => NFA a s1 -> [a] -> Expectation
shouldNotAccept automaton w = w `shouldNotSatisfy` (accepts automaton)


