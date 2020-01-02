#include "space_age.h"

using namespace std;

namespace space_age {
  map<Planet, double> space_age::ORBITAL_FACTORS = {
    {MERCURY, 0.2408467},
    {VENUS, 0.61519726},
    {EARTH, 1.0},
    {MARS, 1.8808158},
    {JUPITER, 11.862615},
    {SATURN, 29.447498},
    {URANUS, 84.016846},
    {NEPTUNE, 164.79132}
  };

  space_age::space_age(long int seconds) : age(seconds) { }

  long int space_age::seconds() const {
    return age;
  }

  double space_age::on_mercury() const {
    return on(MERCURY);
  }

  double space_age::on_venus() const {
    return on(VENUS);
  }

  double space_age::on_earth() const {
    return on(EARTH);
  }

  double space_age::on_mars() const {
    return on(MARS);
  }

  double space_age::on_jupiter() const {
    return on(JUPITER);
  }

  double space_age::on_saturn() const {
    return on(SATURN);
  }

  double space_age::on_uranus() const {
    return on(URANUS);
  }

  double space_age::on_neptune() const {
    return on(NEPTUNE);
  }

  double space_age::on(Planet planet) const {
    return age / EARTH_ORBITAL_PERIOD / ORBITAL_FACTORS[planet];
  }
}
