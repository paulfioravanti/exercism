use std::collections::HashSet;

// usize used since that's what len() returns.
const NUMBER_OF_LETTERS_IN_ALPHABET: usize = 26;

/// Determine whether a sentence is a pangram.
pub fn is_pangram(sentence: &str) -> bool {
    sentence
        .to_lowercase()
        .chars()
        .filter(|c| c.is_ascii_alphabetic())
        .collect::<HashSet<char>>()
        .len()
        == NUMBER_OF_LETTERS_IN_ALPHABET
}
