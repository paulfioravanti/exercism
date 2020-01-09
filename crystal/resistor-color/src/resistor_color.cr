module ResistorColor
  extend self

  private COLORS = StaticArray[
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

  def color_code(color : String) : Int32?
    COLORS.index(color)
  end

  def colors : Array(String)
    COLORS.to_a
  end
end
