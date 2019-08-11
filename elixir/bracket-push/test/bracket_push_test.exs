defmodule BracketPushTest do
  use ExUnit.Case

  test "paired square brackets" do
    assert BracketPush.check_brackets("[]")
  end

  test "empty string" do
    assert BracketPush.check_brackets("")
  end

  test "unpaired brackets" do
    refute BracketPush.check_brackets("[[")
  end

  test "wrong ordered brackets" do
    refute BracketPush.check_brackets("}{")
  end

  test "wrong closing bracket" do
    refute BracketPush.check_brackets("{]")
  end

  test "paired with whitespace" do
    assert BracketPush.check_brackets("{ }")
  end

  test "simple nested brackets" do
    assert BracketPush.check_brackets("{[]}")
  end

  test "several paired brackets" do
    assert BracketPush.check_brackets("{}[]")
  end

  test "paired and nested brackets" do
    assert BracketPush.check_brackets("([{}({}[])])")
  end

  test "unopened closing brackets" do
    refute BracketPush.check_brackets("{[)][]}")
  end

  test "unpaired and nested brackets" do
    refute BracketPush.check_brackets("([{])")
  end

  test "paired and wrong nested brackets" do
    refute BracketPush.check_brackets("[({]})")
  end

  test "math expression" do
    assert BracketPush.check_brackets("(((185 + 223.85) * 15) - 543)/2")
  end

  test "complex latex expression" do
    assert BracketPush.check_brackets(
             "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)"
           )
  end
end
