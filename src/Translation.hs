{-# LANGUAGE LambdaCase #-}
module Translation where

import Control.Monad
import Data.HashMap (Map)
import qualified Data.HashMap as Map
import Data.List.Split
import Data.Maybe

import Rna
import Protein


data Result
  = Ok AminoAcid
  | Stop
  deriving (Eq, Show)

newtype Codon = Codon
  { getCodon :: (RnaBase, RnaBase, RnaBase) }

translateCodon :: Codon -> Result
translateCodon (Codon c) = case c of
  (C, C, _) -> Ok Proline
  (G, C, _) -> Ok Alanine
  (A, C, _) -> Ok Threonine
  (U, C, _) -> Ok Serine
  (A, G, U) -> Ok Serine
  (A, G, C) -> Ok Serine
  (A, G, G) -> Ok Arginine
  (A, G, A) -> Ok Arginine
  (C, G, _) -> Ok Arginine
  (G, G, _) -> Ok Glycine
  (U, G, U) -> Ok Cysteine
  (U, G, C) -> Ok Cysteine
  (U, G, G) -> Ok Tryptophan
  (U, G, A) -> Stop
  (U, A, A) -> Stop
  (U, A, G) -> Stop
  (U, A, C) -> Ok Tyrosine
  (U, A, U) -> Ok Tyrosine
  (A, A, U) -> Ok Asparagine
  (A, A, C) -> Ok Asparagine
  (A, A, G) -> Ok Lysine
  (A, A, A) -> Ok Lysine
  (G, A, A) -> Ok Glutamate
  (G, A, G) -> Ok Glutamate
  (G, A, C) -> Ok Aspartate
  (G, A, U) -> Ok Aspartate
  (C, A, U) -> Ok Histidine
  (C, A, C) -> Ok Histidine
  (C, A, G) -> Ok Glutamine
  (C, A, A) -> Ok Glutamine
  (A, U, G) -> Ok Methionine
  (A, U, A) -> Ok Isoleucine
  (A, U, C) -> Ok Isoleucine
  (A, U, U) -> Ok Isoleucine
  (G, U, _) -> Ok Valine
  (C, U, _) -> Ok Leucine
  (U, U, A) -> Ok Leucine
  (U, U, G) -> Ok Leucine
  (U, U, C) -> Ok Phenylalanine
  (U, U, U) -> Ok Phenylalanine

codonize :: [RnaBase] -> Maybe Codon
codonize [a, b, c] = Just $ Codon (a, b, c)
codonize _ = Nothing

translateRaw :: Rna -> [Result]
translateRaw (Rna rna) = catMaybes . map tr $ chunksOf 3 rna
  where
    tr = liftM translateCodon . codonize

resultToMaybe :: Result -> Maybe AminoAcid
resultToMaybe = \case
    Ok x -> Just x
    Stop -> Nothing

x & f = f x

translate :: Rna -> Protein
translate rna = Protein
  $ translateRaw rna
  & dropWhile (/= Ok Methionine)
  & takeWhile (/= Stop)
  & map resultToMaybe
  & foldr (maybe id (:)) []
