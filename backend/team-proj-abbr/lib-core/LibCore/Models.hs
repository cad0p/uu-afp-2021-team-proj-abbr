{-|
Description : The Models module contains the primary types of the project
Copyright   : Copyright (c) 2022 Pier Carlo Cadoppi, Dmitrii Orlov, Wilmer Zwietering
License     : BSD3
Maintainer  : p.c.cadoppi@students.uu.nl; d.orlov@student.tue.nl; w.j.zwietering@students.uu.nl
Stability   : experimental
-}

module LibCore.Models where

-- | We parse a string into a NoToken if it does not match the syntax for
-- | parsing. The DoMap is a string that has to be mapped. In our proposals,
-- | the DoMap strings start with @@
data Token
  = NoToken String
  | DoMap Keyword
  deriving (Eq, Ord, Show)

-- | Abstract and parameterised Keyword datatype.
data AKeyword a
  = Keyword
      { keyword :: a
      , plural  :: Bool
      }
  deriving (Eq, Ord, Show)

instance Functor AKeyword where
  fmap f (Keyword k p) = Keyword (f k) p

instance Applicative AKeyword where
  pure a = Keyword a False
  (Keyword f p) <*> (Keyword a p') = Keyword (f a) (p && p') -- joining both plurals.

-- | A Keyword is a Token that can be mapped. A Keyword can be plural
type Keyword = AKeyword String

-- | Reserved domain-level error indications.
newtype Error
  = StandardError String
  deriving (Show)
