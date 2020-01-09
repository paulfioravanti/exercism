module ResistorColorDuo
  extend self

  private RESISTOR_VALUES = {
    "black":  "0",
    "brown":  "1",
    "red":    "2",
    "orange": "3",
    "yellow": "4",
    "green":  "5",
    "blue":   "6",
    "violet": "7",
    "grey":   "8",
    "white":  "9",
  }

  def value(bands : Array(String)) : Int
    bands
      .reduce("", &->add_resistor_value(String, String))
      .to_i
  end

  private def add_resistor_value(acc : String, band : String) : String
    acc + RESISTOR_VALUES[band]
  end
end
