-- Based on http://www.haskell.org/haskellwiki/Quasiquotation
module Automata.Regex.QQ where

import Automata.Regex.Data
import Automata.Regex.Compiler
import Automata.Regex.Parser

import Data.Generics.Aliases
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

regex :: QuasiQuoter
regex = QuasiQuoter {quoteExp = quoteRegexExp}

quoteRegexExp :: String -> Q Exp
quoteRegexExp s = do
  loc <- location
  let pos =  (TH.loc_filename loc,
             fst (TH.loc_start loc),
             snd (TH.loc_start loc))
  regex <- parseRegexInContext pos s
  dataToExpQ (const Nothing `extQ` antiRegexExp) regex

antiRegexExp :: Regex Char String -> Maybe (Q Exp)
antiRegexExp (Feature name) = Just $ appE (conE (mkName "Feature")) (varE (mkName name))
antiRegexExp _ = Nothing
