# frozen_string_literal: true

class ResistorColorTrio
  RESISTOR_VALUES = {
    "black" => "0",
    "brown" => "1",
    "red" => "2",
    "orange" => "3",
    "yellow" => "4",
    "green" => "5",
    "blue" => "6",
    "violet" => "7",
    "grey" => "8",
    "white" => "9"
  }.freeze
  private_constant :RESISTOR_VALUES

  def initialize(bands)
    @resistor_value = initialize_resistor_value(bands)
  end

  def label
    value =
      if resistor_value > 1000
        (resistor_value / 1000).to_s + " kiloohms"
      else
        resistor_value.to_s + " ohms"
      end

    "Resistor value: " + value
  end

  private

  attr_reader :resistor_value

  def initialize_resistor_value(bands)
    first, second, third = bands.map(&method(:fetch_resistor_value))
    (first + second).to_i * 10**third.to_i
  end

  def fetch_resistor_value(band)
    RESISTOR_VALUES.fetch(band)
  rescue KeyError
    raise ArgumentError
  end
end
