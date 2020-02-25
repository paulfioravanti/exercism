const NON_ACRONYM_TARGET = new RegExp([
  "(?<=",       // Using a positive lookbehind,
  "[a-zA-Z])",  // *after* an existing letter,
  "[a-zA-Z']+", // find one or more letters or apostrophes
  "|",          // OR
  "[^a-zA-Z]*"  // (if there is no existing letter) zero or more non-letters
].join(""), "g")

export const parse = phrase => {
  return phrase.replace(NON_ACRONYM_TARGET, "").toUpperCase()
}
