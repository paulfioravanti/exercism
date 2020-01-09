module ResistorColorDuo
  extend self

  private RESISTOR_VALUES = StaticArray[
    "black",
    "brown",
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet",
    "grey",
    "white",
  ]

  def value(bands : Array(String)) : Int
    bands.reduce(0, &->add_resistor_value(Int32, String))
  end

  private def add_resistor_value(acc : Int, band : String) : Int
    (acc * 10) + (RESISTOR_VALUES.index(band) || 0)
  end
end
