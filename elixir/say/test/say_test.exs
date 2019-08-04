defmodule SayTest do
  use ExUnit.Case

  # @tag :pending
  test "zero" do
    assert Say.in_english(0) == {:ok, "zero"}
  end

  test "one" do
    assert Say.in_english(1) == {:ok, "one"}
  end

  test "fourteen" do
    assert Say.in_english(14) == {:ok, "fourteen"}
  end

  test "twenty" do
    assert Say.in_english(20) == {:ok, "twenty"}
  end

  test "twenty-two" do
    assert Say.in_english(22) == {:ok, "twenty-two"}
  end

  test "one hundred" do
    assert Say.in_english(100) == {:ok, "one hundred"}
  end

  test "one hundred twenty-three" do
    assert Say.in_english(123) == {:ok, "one hundred twenty-three"}
  end

  test "one thousand" do
    assert Say.in_english(1_000) == {:ok, "one thousand"}
  end

  test "one thousand two hundred thirty-four" do
    assert Say.in_english(1_234) == {:ok, "one thousand two hundred thirty-four"}
  end

  test "one million" do
    assert Say.in_english(1_000_000) == {:ok, "one million"}
  end

  test "one million two thousand three hundred forty-five" do
    assert Say.in_english(1_002_345) == {:ok, "one million two thousand three hundred forty-five"}
  end

  test "ten million three hundred twenty-one thousand one hundred twenty-three" do
    assert Say.in_english(10_321_123) ==
             {:ok, "ten million three hundred twenty-one thousand one hundred twenty-three"}
  end

  test "fifty-four million three hundred twenty-one thousand one hundred twenty-three" do
    assert Say.in_english(54_321_123) ==
             {:ok,
              "fifty-four million three hundred twenty-one thousand one hundred twenty-three"}
  end

  test "six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three" do
    assert Say.in_english(654_321_123) ==
             {:ok,
              "six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three"}
  end

  test "one billion" do
    assert Say.in_english(1_000_000_000) == {:ok, "one billion"}
  end

  test "a big number" do
    assert Say.in_english(987_654_321_123) ==
             {:ok,
              "nine hundred eighty-seven billion six hundred fifty-four million three hundred twenty-one thousand one hundred twenty-three"}
  end

  test "numbers below zero are out of range" do
    assert Say.in_english(-1) == {:error, "number is out of range"}
  end

  test "numbers above 999,999,999,999 are out of range" do
    assert Say.in_english(1_000_000_000_000) == {:error, "number is out of range"}
  end
end
