defmodule ResistorColorTest do
  use ExUnit.Case

  test "returns black color code" do
    assert ResistorColor.code("black") == 0
  end

  test "returns brown color code" do
    assert ResistorColor.code("brown") == 1
  end

  test "returns red color code" do
    assert ResistorColor.code("red") == 2
  end

  test "returns orange color code" do
    assert ResistorColor.code("orange") == 3
  end

  test "returns yellow color code" do
    assert ResistorColor.code("yellow") == 4
  end

  test "returns green color code" do
    assert ResistorColor.code("green") == 5
  end

  test "returns blue color code" do
    assert ResistorColor.code("blue") == 6
  end

  test "returns violet color code" do
    assert ResistorColor.code("violet") == 7
  end

  test "returns grey color code" do
    assert ResistorColor.code("grey") == 8
  end

  test "returns white color code" do
    assert ResistorColor.code("white") == 9
  end

  test "returns all colors" do
    colors = [
      "black",
      "brown",
      "red",
      "orange",
      "yellow",
      "green",
      "blue",
      "violet",
      "grey",
      "white"
    ]

    assert ResistorColor.colors() == colors
  end
end
