{-# LANGUAGE FlexibleContexts #-}
module Automata.Regex.Compiler(compileRegex) where

import Control.Monad.Writer
import Control.Monad.State
import Data.DList
import qualified Data.Map as M
import Control.Monad
import Automata.Regex.Data
import Automata.NFA

compileRegex :: (Ord a, Language l a, Show a) => [a] -> (Regex a l) -> NFA a Int
compileRegex alphabet regex = alphabetProjection [()] $ makeAutomaton [init] transitions [acc] where
  ((init,acc), transitions') = runWriter $ (flip evalStateT) 0 $ process alphabet regex
  transitions = toList transitions'

type TransitionWithEps a = (Either () a, Int, Int)

-- this function takes a regexp destription and produces an automaton (as the list of its transitions
-- on the Writers monad output) with one accepting and one initial state. The returned values are
-- the numbers of consequentially initial and accepting state.
process :: (MonadWriter (DList (TransitionWithEps a)) m, Show a, MonadState Int m, Ord a, Language l a) => [a] -> Regex a l -> m (Int, Int)
process _ (Letter x) = do
  init <- getNewId
  acc <- getNewId
  emitTransition x init acc
  return (init, acc)
process alphabet (Concat a1 a2) = do
  (init1, acc1) <- process alphabet a1
  (init2, acc2) <- process alphabet a2
  emitEpsTransition acc1 init2
  return (init1, acc2)
process alphabet (Union a1 a2) = do
  (init1, acc1) <- process alphabet a1
  (init2, acc2) <- process alphabet a2
  init <- getNewId
  acc <- getNewId
  emitEpsTransition init init1
  emitEpsTransition init init2
  emitEpsTransition acc1 acc
  emitEpsTransition acc2 acc
  return (init, acc)
process alphabet (Iterate r) = do
  (init, acc) <- process alphabet r
  emitEpsTransition acc init
  return (init, init)
process alphabet (Feature f) = do
  let epsAlphabet = (Right <$> alphabet) ++ [Left ()]
  let automaton = normaliseAutomaton epsAlphabet $ makeOneInitialAndOneAcceptingState $ toAutomaton alphabet f
  let transitions = getTransitions epsAlphabet automaton
  let allNodes = [s | (_, s, _) <- transitions] ++ [s | (_, _, s) <- transitions] ++ initialStates automaton ++ getAcceptingStates automaton
  let updateMap mapping newNum = if not $ M.member newNum mapping then (do {newId <- getNewId; return $ M.insert newNum newId mapping}) else return mapping
  translation <- foldM updateMap M.empty allNodes
  let (!?) = flip M.lookup
  forM_ transitions (\(l, s1, s2) -> do
     let (Just s1id) = translation !? s1
     let (Just s2id) = translation !? s2
     tell $ fromList [(l, s1id, s2id)])
  let [initial] = initialStates automaton
  let [accepting] = getAcceptingStates automaton
  let (Just initial')= translation !? initial
  let (Just accepting') = translation !? accepting
  return (initial', accepting')


getNewId :: (MonadState Int m) => m Int
getNewId = do
  s <- get
  put (s+1)
  return s

emitTransition :: (MonadWriter (DList (TransitionWithEps a)) m) => a -> Int -> Int -> m ()
emitTransition a s1 s2 = tell $ fromList [(Right a,s1,s2)]

emitEpsTransition :: (MonadWriter (DList (TransitionWithEps a)) m) => Int -> Int -> m ()
emitEpsTransition s1 s2 = tell $ fromList [(Left (), s1, s2)]
