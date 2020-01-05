from typing import Set
import string


__ALPHABET: Set[str] = set(string.ascii_lowercase)


def is_pangram(sentence: str) -> bool:
    unique_letters = set(sentence.lower())
    return __ALPHABET.issubset(unique_letters)
