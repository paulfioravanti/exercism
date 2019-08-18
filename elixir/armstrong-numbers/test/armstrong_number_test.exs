defmodule ArmstrongNumberTest do
  use ExUnit.Case

  test "One-digit numbers are Armstrong numbers" do
    assert ArmstrongNumber.valid?(1)
    assert ArmstrongNumber.valid?(6)
    assert ArmstrongNumber.valid?(9)
  end

  test "Two-digit numbers aren't Armstrong numbers" do
    refute ArmstrongNumber.valid?(11)
    refute ArmstrongNumber.valid?(89)
    refute ArmstrongNumber.valid?(44)
  end

  test "Three-digit numbers that are the Armstrong numbers" do
    assert ArmstrongNumber.valid?(153)
    assert ArmstrongNumber.valid?(370)
  end

  test "Three-digit numbers that aren't the Armstrong numbers" do
    refute ArmstrongNumber.valid?(555)
    refute ArmstrongNumber.valid?(662)
  end

  test "Four-digit number that is the Armstrong numbers" do
    assert ArmstrongNumber.valid?(1634)
  end

  test "Four-digit number that is not the Armstrong numbers" do
    refute ArmstrongNumber.valid?(9989)
  end

  test "Seven-digit number that is the Armstrong numbers" do
    assert ArmstrongNumber.valid?(9_926_315)
  end

  test "Seven-digit number that is not the Armstrong numbers" do
    refute ArmstrongNumber.valid?(2_369_989)
  end

  test "Ten-digit number that is the Armstrong numbers" do
    assert ArmstrongNumber.valid?(4_679_307_774)
  end

  test "Fourteen-digit number that is the Armstrong numbers" do
    assert ArmstrongNumber.valid?(28_116_440_335_967)
  end
end
