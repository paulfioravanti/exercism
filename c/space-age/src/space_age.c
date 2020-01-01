#include "space_age.h"

float convert_planet_age(planet_t planet, int64_t input) {
  return input / EARTH_ORBITAL_PERIOD / ORBITAL_FACTORS[planet];
}
