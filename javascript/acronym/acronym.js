// Target the following for removal from the phrase:
// 1. One or more letters or apostrophes *after* an existing letter
//    (found using positive lookbehind) OR
// 2. (If there is no existing letter) Zero or more non-letters
const NON_ACRONYM_TARGET = /(?<=[a-zA-Z])[a-zA-Z']+|[^a-zA-Z]*/g

export const parse = phrase => {
  return phrase.replace(NON_ACRONYM_TARGET, "").toUpperCase()
}
