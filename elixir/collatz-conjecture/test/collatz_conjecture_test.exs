defmodule CollatzConjectureTest do
  use ExUnit.Case

  test "zero steps for one" do
    assert CollatzConjecture.calc(1) == 0
  end

  test "zero is an error" do
    assert_raise FunctionClauseError, fn -> CollatzConjecture.calc(0) end
  end

  test "divide if even" do
    assert CollatzConjecture.calc(16) == 4
  end

  test "even and odd steps" do
    assert CollatzConjecture.calc(12) == 9
  end

  test "Large number of even and odd steps" do
    assert CollatzConjecture.calc(1_000_000) == 152
  end

  test "start with odd step" do
    assert CollatzConjecture.calc(21) == 7
  end

  test "more steps than starting number" do
    assert CollatzConjecture.calc(7) == 16
  end

  test "negative value is an error " do
    assert_raise FunctionClauseError, fn -> CollatzConjecture.calc(-15) end
  end

  test "string as input value is an error " do
    assert_raise FunctionClauseError, fn -> CollatzConjecture.calc("fubar") end
  end
end
