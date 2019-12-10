defmodule RunLengthEncoderTest do
  use ExUnit.Case

  test "encode empty string" do
    assert RunLengthEncoder.encode("") === ""
  end

  test "encode single characters only are encoded without count" do
    assert RunLengthEncoder.encode("XYZ") === "XYZ"
  end

  test "encode string with no single characters" do
    assert RunLengthEncoder.encode("AABBBCCCC") == "2A3B4C"
  end

  test "encode single characters mixed with repeated characters" do
    assert RunLengthEncoder.encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB") ===
             "12WB12W3B24WB"
  end

  test "encode multiple whitespace mixed in string" do
    assert RunLengthEncoder.encode("  hsqq qww  ") === "2 hs2q q2w2 "
  end

  test "encode lowercase characters" do
    assert RunLengthEncoder.encode("aabbbcccc") === "2a3b4c"
  end

  test "decode empty string" do
    assert RunLengthEncoder.decode("") === ""
  end

  test "decode single characters only" do
    assert RunLengthEncoder.decode("XYZ") === "XYZ"
  end

  test "decode string with no single characters" do
    assert RunLengthEncoder.decode("2A3B4C") == "AABBBCCCC"
  end

  test "decode single characters with repeated characters" do
    assert RunLengthEncoder.decode("12WB12W3B24WB") ===
             "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
  end

  test "decode multiple whitespace mixed in string" do
    assert RunLengthEncoder.decode("2 hs2q q2w2 ") === "  hsqq qww  "
  end

  test "decode lower case string" do
    assert RunLengthEncoder.decode("2a3b4c") === "aabbbcccc"
  end

  test "encode followed by decode gives original string" do
    original = "zzz ZZ  zZ"
    encoded = RunLengthEncoder.encode(original)
    assert RunLengthEncoder.decode(encoded) === original
  end
end
