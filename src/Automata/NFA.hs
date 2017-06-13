{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE FlexibleContexts #-}

module Automata.NFA where

import GHC.Exts hiding (Any)
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as F

data NFA alfabet states = NFA { nextState :: states -> alfabet -> [states]
                              , initialStates :: [states]
                              , isAccepting :: states -> Any
                              , allStates :: [states]
                              }

instance Show (NFA a b) where
  show _ = "NFA"

accepts :: NFA a s -> [a] -> Bool
accepts NFA{..} word = getAny $ mconcat $ do
  s <- initialStates
  isAccepting <$> foldM nextState s word

acceptsFast :: (Ord s) => NFA a s -> [a] -> Bool
acceptsFast NFA{..} word = getAny $ foldMap isAccepting $
    foldl (\states letter -> S.toList $ S.fromList $ concatMap ((flip nextState) letter) states) initialStates word


makeAutomaton  :: (Ord a, Ord s)
               => [s] -- ^ Initial states
               -> [(a, s, s)] -- ^ Transitions
               -> [s] -- ^ Accepting states
               -> NFA a s
makeAutomaton initialStates transitions accepting = NFA{..}
  where
    isAccepting x = Any $ S.member x $ S.fromList accepting
    groupedTransitions = [((the letter, the from), to) | (letter, from, to) <- transitions, then group by (letter, from) using groupWith]
    nextState state letter = F.concat $ M.lookup (letter, state) $ M.fromList groupedTransitions
    allStates = S.elems $ S.fromList $ [x | (_, x, _) <- transitions] ++ [y | (_, _, y) <- transitions]

getTransitions :: [a] -> NFA a s -> [(a,s,s)]
getTransitions alphabet automaton = do
  s1 <- (allStates automaton)
  letter <- alphabet
  s2 <- (nextState automaton s1 letter)
  return (letter, s1, s2)

getAcceptingStates :: NFA a s -> [s]
getAcceptingStates automaton = [s | s <- (allStates automaton), (\(Any x) -> x) $ isAccepting automaton s]

unionAutomaton :: NFA a s1 -> NFA a s2 -> NFA a (Either s1 s2)
unionAutomaton n1 n2 = (NFA nextState' initialStates' isAccepting' allStates')
  where
    isAccepting' (Left x) = isAccepting n1 x
    isAccepting' (Right x) = isAccepting n2 x
    nextState' (Left x) l = Left <$> (nextState n1 x l)
    nextState' (Right x) l = Right <$> (nextState n2 x l)
    disjointUnion l1 l2 = (map Left l1) ++ (map Right l2)
    initialStates' = disjointUnion (initialStates n1) (initialStates n2)
    allStates' = disjointUnion (allStates n1) (allStates n2)


intersectionAutomaton :: NFA a s1 -> NFA a s2 -> NFA a (s1, s2)
intersectionAutomaton n1 n2  = (NFA nextState' initialStates' isAccepting' allStates')
  where
    cartesianProduct x y = [(ex, ey) | ex <- x, ey <- y]
    nextState' (s1, s2) l = cartesianProduct (nextState n1 s1 l) (nextState n2 s2 l)
    isAccepting' (s1, s2) = Any $ (getAny $ isAccepting n1 s1) && (getAny $ isAccepting n2 s2)
    allStates' = cartesianProduct (allStates n1) (allStates n2)
    initialStates' = cartesianProduct (initialStates n1) (initialStates n2)

reachableStates :: (Ord s) => [a] -> NFA a s -> [s]
reachableStates alphabet NFA{..} = S.elems $ flip execState S.empty $ forM_ initialStates dfs
  where
    dfs s = do
      visited <- get
      unless (s `S.member` visited) $ do
        put $ S.insert s visited
        forM_ alphabet $ (\a -> mapM_ dfs $ nextState s a)

normaliseAutomaton :: (Ord a, Ord s) => [a] -> NFA a s -> NFA a Int
normaliseAutomaton alphabet n1@NFA{..} = makeAutomaton initialStates' transitions accepting
  where
    (!) = (M.!)
    (!?) = flip M.lookup
    runJust (Just x) = x
    mapping = M.fromList $ zip (reachableStates alphabet n1) [0..]
    allStates' = snd <$> zip allStates [0..]
    initialStates' = (mapping !) <$> initialStates
    acceptingStatesList = [s | s <- allStates, getAny $ isAccepting s]
    accepting = catMaybes ((mapping !?) <$> acceptingStatesList)
    transitions = do
      s1 <- allStates
      a <- alphabet
      s2 <- nextState s1 a
      return (a, runJust (mapping !? s1), runJust (mapping !? s2))

isEmpty :: (Ord s) => [a] -> NFA a s -> Bool
isEmpty alphabet n1@NFA{..} = not $ getAny $ mconcat $ isAccepting <$> reachableStates alphabet n1

{--| Simply remove the `Left` edges from the automaton --}
filterAlphabet :: NFA (Either x a) b -> NFA a b
filterAlphabet n1 = n1 {nextState=nextState'} where
  nextState' state letter = (nextState n1) state (Right letter)

{--| Accept a word iff you can add some `Left` letters into it, so that the original automaton accepts it --}
alphabetProjection :: (Ord b) => [eps] -> NFA (Either eps a) b -> NFA a b
alphabetProjection epses n1 = n1 {nextState=nextState', initialStates=initialStates', isAccepting=isAccepting'} where
  leftNextState _ (Right _) = []
  leftNextState state letter = (nextState n1) state letter
  initialStates' = reachableStates (Left <$> epses) $ n1 {nextState=leftNextState}
  nextState' state letter = S.toList $ S.fromList $ concatMap ((flip (nextState n1)) (Right letter)) reachableByLeft
    where reachableByLeft = reachableStates (Left <$> epses) $ n1 {initialStates=[state], nextState=leftNextState}
  isAccepting' state = foldMap (isAccepting n1) $ reachableStates (Left <$> epses) $ n1 {initialStates=[state], nextState=leftNextState}

stateIsomorphism :: (a -> b) -> (b -> a) -> NFA l a -> NFA l b
stateIsomorphism f finv automaton = (NFA nextState' initialStates' isAccepting' allStates')
  where
    nextState' state l = f <$> (nextState automaton) (finv state) l
    initialStates' = f <$> initialStates automaton
    isAccepting' s = (isAccepting automaton) (finv s)
    allStates' = f <$> allStates automaton

makeOneInitialAndOneAcceptingState :: NFA a s -> NFA (Either () a) (Either Bool s)
makeOneInitialAndOneAcceptingState aut = (NFA nextState' initialStates' isAccepting' allStates')
  where
    allStates' = (Right <$> allStates aut) ++ [Left True, Left False]
    initialStates' = [Left True]
    isAccepting' (Left False) = Any True
    isAccepting' _ = Any False
    nextState' (Right x) (Right l) = Right <$> nextState aut x l
    nextState' (Right x) (Left ()) = if (\(Any x) -> x) $ isAccepting aut x then [Left False] else []
    nextState' (Left True) (Left ()) = Right <$> initialStates aut
    nextState' _ _ = []
