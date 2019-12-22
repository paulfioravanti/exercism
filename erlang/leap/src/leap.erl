-module(leap).

-export([leap_year/1]).

-define(LEAP_YEAR, 4).
-define(CENTURIAL_YEAR, 100).
-define(LEAP_CYCLE_LENGTH, (?LEAP_YEAR * ?CENTURIAL_YEAR)).


leap_year(Year) ->
  is_leap(Year) andalso (is_non_centurial(Year) orelse is_leap_cycle(Year)).

% Private

is_leap(Year) ->
  Year rem ?LEAP_YEAR == 0.

is_non_centurial(Year) ->
  Year rem ?CENTURIAL_YEAR /= 0.

is_leap_cycle(Year) ->
  Year rem ?LEAP_CYCLE_LENGTH == 0.
