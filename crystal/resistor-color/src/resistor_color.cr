module ResistorColor
  extend self

  private alias MaybeInt = Int32 | Nil

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

  def color_code(color : String) : MaybeInt
    COLORS.index(color)
  end

  def colors : Array(String)
    COLORS.to_a
  end
end
