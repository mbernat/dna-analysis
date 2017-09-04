module Dna where

import Data.List
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)


data DnaBase = A | C | G | T
  deriving (Show)

newtype Dna = Dna { getDna :: [DnaBase] }

instance Show Dna where
  show (Dna l) = concatMap show l

base :: ReadP DnaBase
base = choice
  [ satisfy (\c -> c == 'a' || c == 'A') >> pure A
  , satisfy (\c -> c == 'c' || c == 'C') >> pure C
  , satisfy (\c -> c == 'g' || c == 'G') >> pure G
  , satisfy (\c -> c == 't' || c == 'T') >> pure T
  ]

dna :: ReadP Dna
dna = Dna <$> many base

instance Read Dna where
  readPrec = ReadPrec.lift dna
