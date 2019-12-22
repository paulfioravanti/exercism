-module(rna_transcription).

-export([to_rna/1]).

-define(RNA_TRANSCRIPTIONS, #{$G => $C, $C => $G, $T => $A, $A => $U}).


to_rna(Strand) ->
  lists:map(fun fetch_rna/1, Strand).

% Private

fetch_rna(Dna) ->
  maps:get(Dna, ?RNA_TRANSCRIPTIONS).
