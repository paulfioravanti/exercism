-module(space_age).

-export([age/2]).

-define(EARTH_ORBITAL_PERIOD, 31557600).
-define(ORBITAL_FACTORS, #{earth => 1.0,
                           mercury => 0.2408467,
                           venus => 0.61519726,
                           mars => 1.8808158,
                           jupiter => 11.862615,
                           saturn => 29.447498,
                           uranus => 84.016846,
                           neptune => 164.79132}).


age(Planet, Seconds) ->
  Seconds / ?EARTH_ORBITAL_PERIOD / maps:get(Planet, ?ORBITAL_FACTORS).

