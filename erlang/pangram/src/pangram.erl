-module(pangram).

-export([is_pangram/1]).

-define(ALPHABET, lists:seq($a, $z)).


is_pangram(Sentence) ->
  InSentence = in_sentence(Sentence),
  lists:all(InSentence, ?ALPHABET).

% Private

in_sentence(Sentence) ->
  LowerSentence = string:lowercase(Sentence),
  fun(Character) -> lists:member(Character, LowerSentence) end.
