from typing import Dict, List
from enum import Enum


class Dna(Enum):
    C = "C"
    G = "G"
    A = "A"
    T = "T"


class Rna(Enum):
    G = "G"
    C = "C"
    U = "U"
    A = "A"


__RNA_TRANSCRIPTIONS: Dict[Dna, Rna] = {
    Dna.C: Rna.G,
    Dna.G: Rna.C,
    Dna.A: Rna.U,
    Dna.T: Rna.A
}


def to_rna(dna_strand: str) -> str:
    rna_nucleotides: List[str] = list(
        map(__translate_nucleotide, list(dna_strand))
    )
    return "".join(rna_nucleotides)


def __translate_nucleotide(nucleotide: str) -> str:
    return Rna(__RNA_TRANSCRIPTIONS[Dna(nucleotide)]).value
