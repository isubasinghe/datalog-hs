{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Function (fix)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)

data Rule = Rule {_head :: !Atom, _body :: ![Atom]}

data Atom = Atom {_predSym :: !String, _terms :: ![Term]} deriving (Eq)

data Term = Var !String | Sym !String deriving (Eq)

type Program = [Rule]

type KnowledgeBase = [Atom]

type Substitution = [(Term, Term)]

emptySubstitution :: Substitution
emptySubstitution = []

substitute :: Atom -> Substitution -> Atom
substitute atom substitution = atom {_terms = map go (_terms atom)}
  where
    go sym@Sym {} = sym
    go var@Var {} = fromMaybe var (var `lookup` substitution)

unify :: Atom -> Atom -> Maybe Substitution
unify (Atom predSym ts) (Atom predSym' ts')
  | predSym == predSym' = go $ zip ts ts'
  | otherwise = Nothing
  where
    go :: [(Term, Term)] -> Maybe Substitution
    go [] = Just emptySubstitution
    go ((s@Sym {}, s'@Sym {}) : rest) = if s == s' then go rest else Nothing
    go ((v@Var {}, s@Sym {}) : rest) = do
      incompleteSubstitution <- go rest
      case v `lookup` incompleteSubstitution of
        Just s' | s /= s' -> Nothing
        _anyOtherFailure -> return $ (v, s) : incompleteSubstitution
    go ((_, Var {}) : _) = error "The second atom is assumed to be ground."

evalAtom :: KnowledgeBase -> Atom -> [Substitution] -> [Substitution]
evalAtom kb atom substitutions = do
  substitution <- substitutions
  let downToEarthAtom = substitute atom substitution
  extension <- mapMaybe (unify downToEarthAtom) kb
  return $ substitution <> extension

walk :: KnowledgeBase -> [Atom] -> [Substitution]
walk kb = foldr (evalAtom kb) [emptySubstitution]

evalRule :: KnowledgeBase -> Rule -> KnowledgeBase
evalRule kb (Rule head body) = map (substitute head) (walk kb body)

isRangeRestricted :: Rule -> Bool
isRangeRestricted Rule {..} =
  vars _head `isSubsetOf` concatMap vars _body
  where
    isSubsetOf as bs = all (`elem` bs) as
    vars Atom {..} = nub $ filter (\case Var {} -> True; _other -> False) _terms

immediateConsequence :: Program -> KnowledgeBase -> KnowledgeBase
immediateConsequence rules kb =
  nub . (kb <>) . concatMap (evalRule kb) $ rules

solve :: Program -> KnowledgeBase
solve rules =
  if all isRangeRestricted rules
    then fix step []
    else error "The input program is not range-restricted."
  where
    step ::
      (KnowledgeBase -> KnowledgeBase) ->
      (KnowledgeBase -> KnowledgeBase)
    step f currentKB
      | nextKB <- immediateConsequence rules currentKB =
          if nextKB == currentKB
            then currentKB
            else f nextKB

main = do
  putStrLn "DONE"
