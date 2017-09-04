{-# LANGUAGE LambdaCase #-}
module Protein where

import Data.HashMap (Map)
import qualified Data.HashMap as Map
import Data.List


data AminoAcid
  = Valine
  | Alanine
  | Leucine
  | Isoleucine
  | Phenylalanine
  | Tyrosine
  | Tryptophan
  | Methionine
  | Glycine
  | Proline
  | Cysteine
  | Serine
  | Threonine
  | Asparagine
  | Glutamine
  | Histidine
  | Lysine
  | Arginine
  | Aspartate
  | Glutamate
  deriving (Eq)

showAminoTriplet = \case
    Valine -> "Val"
    Alanine -> "Ala"
    Leucine -> "Leu"
    Isoleucine -> "Ile"
    Phenylalanine -> "Phe"
    Tyrosine -> "Tyr"
    Tryptophan -> "Trp"
    Methionine -> "Met"
    Glycine -> "Gly"
    Proline -> "Pro"
    Cysteine -> "Cys"
    Serine -> "Ser"
    Threonine -> "Thr"
    Asparagine -> "Asn"
    Glutamine -> "Gln"
    Histidine -> "His"
    Lysine -> "Lys"
    Arginine -> "Arg"
    Aspartate -> "Asp"
    Glutamate -> "Glu"

instance Show AminoAcid where
  show = showAminoTriplet

showAminoSinglet = \case
    Valine -> "V"
    Alanine -> "A"
    Leucine -> "L"
    Isoleucine -> "I"
    Phenylalanine -> "F"
    Tyrosine -> "Y"
    Tryptophan -> "W"
    Methionine -> "M"
    Glycine -> "G"
    Proline -> "P"
    Cysteine -> "C"
    Serine -> "S"
    Threonine -> "T"
    Asparagine -> "N"
    Glutamine -> "Q"
    Histidine -> "H"
    Lysine -> "K"
    Arginine -> "R"
    Aspartate -> "D"
    Glutamate -> "E"

newtype Protein = Protein { getProtein :: [AminoAcid] }

instance Show Protein where
  show (Protein p) = concat $ intersperse "-" $ map show p
