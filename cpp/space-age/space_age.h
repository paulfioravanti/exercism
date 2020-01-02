#if !defined(SPACE_AGE_H)
#define SPACE_AGE_H

#include <string>
#include <map>

enum Planet {
  MERCURY,
  VENUS,
  EARTH,
  MARS,
  JUPITER,
  SATURN,
  URANUS,
  NEPTUNE
};

namespace space_age {
  class space_age {
    private:
      static constexpr double EARTH_ORBITAL_PERIOD = 31557600.0;
      static std::map<Planet, double> ORBITAL_FACTORS;
      const long int age;
      double on(Planet) const;
    public:
      explicit space_age(long int);
      long int seconds() const;
      double on_mercury() const;
      double on_venus() const;
      double on_earth() const;
      double on_mars() const;
      double on_jupiter() const;
      double on_saturn() const;
      double on_uranus() const;
      double on_neptune() const;
  };

}

#endif
