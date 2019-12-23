-module(bob).

-export([response/1]).


response(String) ->
  case string:trim(String) of
    "" ->
      "Fine. Be that way!";

    Remark ->
      respond_to_verbal_remark(Remark)
  end.

% Private

respond_to_verbal_remark(Remark) ->
  case {is_question(Remark), is_yelling(Remark)} of
    {true, true} ->
      "Calm down, I know what I'm doing!";

    {true, false} ->
      "Sure.";

    {false, true} ->
      "Whoa, chill out!";

    {false, false} ->
      "Whatever."
  end.

is_question(Remark) ->
  lists:last(Remark) =:= $?.

is_yelling(Remark) ->
  Remark =:= string:uppercase(Remark) andalso
    Remark =/= string:lowercase(Remark).
