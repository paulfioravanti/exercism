module Space
  class Age
    private enum Planet
      Earth
      Mercury
      Venus
      Mars
      Jupiter
      Saturn
      Uranus
      Neptune
    end
    private EARTH_ORBITAL_PERIOD = 31557600.0
    private ORBITAL_FACTORS      = {
      Planet::Mercury => 0.2408467,
      Planet::Venus   => 0.61519726,
      Planet::Earth   => 1.0,
      Planet::Mars    => 1.8808158,
      Planet::Jupiter => 11.862615,
      Planet::Saturn  => 29.447498,
      Planet::Uranus  => 84.016846,
      Planet::Neptune => 164.79132,
    }

    def self.from_seconds(seconds : Int32) : self
      new(seconds)
    end

    private def initialize(@age : Int32) : Int32
    end

    private getter age : Int32

    private macro define_age_on_planets
      {% for planet, factor in ORBITAL_FACTORS %}
        def age_on_{{planet.names.last.downcase}} : Float64
          age / EARTH_ORBITAL_PERIOD / {{factor}}
        end
      {% end %}
    end

    define_age_on_planets
  end
end
