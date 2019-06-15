# frozen_string_literal: true

class SpaceAge
  EARTH_ORBITAL_PERIOD = 31_557_600.0
  private_constant :EARTH_ORBITAL_PERIOD
  ORBITAL_PERIODS = {
    earth: EARTH_ORBITAL_PERIOD,
    mercury: EARTH_ORBITAL_PERIOD * 0.2408467,
    venus: EARTH_ORBITAL_PERIOD * 0.61519726,
    mars: EARTH_ORBITAL_PERIOD * 1.8808158,
    jupiter: EARTH_ORBITAL_PERIOD * 11.862615,
    saturn: EARTH_ORBITAL_PERIOD * 29.447498,
    uranus: EARTH_ORBITAL_PERIOD * 84.016846,
    neptune: EARTH_ORBITAL_PERIOD * 164.79132
  }.freeze
  private_constant :ORBITAL_PERIODS

  def initialize(age_in_seconds)
    @age_in_seconds = age_in_seconds
  end

  ORBITAL_PERIODS.each do |planet, orbital_period|
    define_method("on_#{planet}") do
      age_in_seconds / orbital_period
    end
  end

  private

  attr_reader :age_in_seconds
end
