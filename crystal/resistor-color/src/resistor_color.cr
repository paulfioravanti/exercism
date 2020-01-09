module ResistorColor
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

  def color_code(color : String) : Int32
    Color.parse(color).value
  end

  def colors : Array(String)
    Color.names.map(&.downcase)
  end
end
