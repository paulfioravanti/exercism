class SpaceAge {
    private enum Planet {
        MERCURY(0.2408467),
        VENUS(0.61519726),
        EARTH(1.0),
        MARS(1.8808158),
        JUPITER(11.862615),
        SATURN(29.447498),
        URANUS(84.016846),
        NEPTUNE(164.7913);

        private final double orbitalFactor;

        private Planet(double orbitalFactor) {
          this.orbitalFactor = orbitalFactor;
        }
    }

    private static final double EARTH_ORBITAL_PERIOD = 31557600.0;
    private final double seconds;

    SpaceAge(double seconds) {
      this.seconds = seconds;
    }

    double getSeconds() {
      return this.seconds;
    }

    double onEarth() {
      return on(Planet.EARTH);
    }

    double onMercury() {
      return on(Planet.MERCURY);
    }

    double onVenus() {
      return on(Planet.VENUS);
    }

    double onMars() {
      return on(Planet.MARS);
    }

    double onJupiter() {
      return on(Planet.JUPITER);
    }

    double onSaturn() {
      return on(Planet.SATURN);
    }

    double onUranus() {
      return on(Planet.URANUS);
    }

    double onNeptune() {
      return on(Planet.NEPTUNE);
    }

    private double on(Planet planet) {
      return this.seconds / EARTH_ORBITAL_PERIOD / planet.orbitalFactor;
    }
}
