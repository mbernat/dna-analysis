{-# LANGUAGE LambdaCase #-}
module Rna where

import Dna (Dna(Dna), DnaBase)
import qualified Dna


data RnaBase = A | C | G | U
  deriving (Eq, Show)

newtype Rna = Rna { getRna :: [RnaBase] }

instance Show Rna where
  show (Rna rna) = concatMap show rna

transcribeBase :: DnaBase -> RnaBase
transcribeBase = \case
  Dna.A -> A
  Dna.C -> C
  Dna.G -> G
  Dna.T -> U
  
transcribe :: Dna -> Rna
transcribe (Dna dna) = Rna $ map transcribeBase dna
