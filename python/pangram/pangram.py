from typing import Pattern
import re


__NON_ASCII_LETTERS: Pattern[str] = re.compile("[^a-z]")
__NUMBER_OF_LETTERS_IN_ALPHABET: int = 26


def is_pangram(sentence: str) -> bool:
    stripped: str = re.sub(__NON_ASCII_LETTERS, "", sentence.lower())
    return len(set(stripped)) == __NUMBER_OF_LETTERS_IN_ALPHABET
