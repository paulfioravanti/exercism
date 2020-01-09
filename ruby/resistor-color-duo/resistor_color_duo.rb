# frozen_string_literal: true

module ResistorColorDuo
  COLORS = %w[
    black
    brown
    red
    orange
    yellow
    green
    blue
    violet
    grey
    white
  ].freeze
  private_constant :COLORS

  module_function

  def value(bands)
    bands.reduce(0, &method(:add_resistor_value))
  end

  def add_resistor_value(acc, band)
    acc * 10 + COLORS.index(band)
  end
  private_class_method :add_resistor_value
end
