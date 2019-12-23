-module(pangram).

-export([is_pangram/1]).

-define(NON_ASCII_LETTERS, "[^a-z]").
-define(NUMBER_OF_LETTERS_IN_ALPHABET, 26).


is_pangram(Sentence) ->
  LowerCased = string:lowercase(Sentence),
  Cleaned = remove_non_letters(LowerCased),
  UniqueLetters = uniq(Cleaned),
  string:length(UniqueLetters) == ?NUMBER_OF_LETTERS_IN_ALPHABET.

% Private

remove_non_letters(Sentence) ->
  re:replace(Sentence, ?NON_ASCII_LETTERS, "", [global, {return, list}]).

uniq(Sentence) ->
  sets:to_list(sets:from_list(Sentence)).
