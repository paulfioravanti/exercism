module ResistorColorDuo
  extend self

  enum Color
    Black
    Brown
    Red
    Orange
    Yellow
    Green
    Blue
    Violet
    Grey
    White
  end

  def value(bands : Array(String)) : Int32
    bands.reduce(0, &->add_resistor_value(Int32, String))
  end

  private def add_resistor_value(acc : Int32, band : String) : Int32
    acc * 10 + Color.parse(band).value
  end
end
