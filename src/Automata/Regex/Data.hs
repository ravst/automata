{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyCase #-}
module Automata.Regex.Data where

import Automata.NFA
import Data.Void
import Data.Typeable
import Data.Data

data Regex a e = Letter a
             | Concat (Regex a e) (Regex a e)
             | Union (Regex a e) (Regex a e)
             | Iterate (Regex a e)
             | Feature e
             deriving(Eq, Show, Functor, Data, Typeable)

class Language language alphabet where
  toAutomaton :: [alphabet] -> language -> NFA alphabet Int

instance (Ord a, Ord s) => Language (NFA a s) a where
  toAutomaton = normaliseAutomaton

instance Language Void a where
  toAutomaton _ x = case x of

