module Raindrops
  CONVERSIONS = {
    3 => "Pling",
    5 => "Plang",
    7 => "Plong"
  }.freeze
  private_constant :CONVERSIONS

  module_function

  def convert(number)
    raindrops = "".tap { |string| add_raindrop_conversion(string, number) }
    raindrops.empty? ? number.to_s : raindrops
  end

  def add_raindrop_conversion(string, number)
    CONVERSIONS.each do |factor, raindrop_speak|
      string << raindrop_speak if (number % factor).zero?
    end
  end
  private_class_method :add_raindrop_conversion
end
