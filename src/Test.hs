module Test where

import Dna (Dna)
import Rna
import Translation

dna :: Dna
dna = read "cgtatggcggggcccttatactgcatgacacgatagcctgtc"

rna = transcribe dna

rawResult = translateRaw rna

result = translate rna
