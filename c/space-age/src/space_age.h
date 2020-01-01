#ifndef SPACE_AGE_H
#define SPACE_AGE_H

#define NUM_PLANETS 8

#include <stdint.h> // int64_t

typedef enum planet {
   MERCURY,
   VENUS,
   EARTH,
   MARS,
   JUPITER,
   SATURN,
   URANUS,
   NEPTUNE,
} planet_t;

static const int EARTH_ORBITAL_PERIOD;

static const float ORBITAL_FACTORS[NUM_PLANETS];

float convert_planet_age(planet_t planet, int64_t input);

#endif
