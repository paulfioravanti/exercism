#include "space_age.h"

static const int EARTH_ORBITAL_PERIOD = 31557600.0;
static const float ORBITAL_FACTORS[NUM_PLANETS] = {
  [MERCURY] = 0.2408467,
  [VENUS] = 0.61519726,
  [EARTH] = 1.0,
  [MARS] = 1.8808158,
  [JUPITER] = 11.862615,
  [SATURN] = 29.447498,
  [URANUS] = 84.016846,
  [NEPTUNE] = 164.79132,
};

float convert_planet_age(planet_t planet, int64_t input) {
  return input / EARTH_ORBITAL_PERIOD / ORBITAL_FACTORS[planet];
}
