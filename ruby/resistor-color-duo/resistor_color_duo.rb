# frozen_string_literal: true

module ResistorColorDuo
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

  module_function

  def value(bands)
    bands
      .reduce("", &method(:add_resistor_value))
      .to_i
  end

  def add_resistor_value(acc, band)
    acc + RESISTOR_VALUES[band]
  end
  private_class_method :add_resistor_value
end
