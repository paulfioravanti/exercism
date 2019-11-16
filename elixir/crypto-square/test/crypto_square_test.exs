defmodule CryptoSquareTest do
  use ExUnit.Case

  test "empty string" do
    assert CryptoSquare.encode("") == ""
  end

  test "perfect square" do
    assert CryptoSquare.encode("abcd") == "ac bd"
  end

  test "uppercase string" do
    assert CryptoSquare.encode("ABCD") == "ac bd"
  end

  test "small imperfect square" do
    assert CryptoSquare.encode("This is easy") == "tis hsy ie sa"
  end

  test "punctuation and numbers" do
    assert CryptoSquare.encode("1, 2, 3, Go! Go, for God's sake!") == "1gga 2ook 3fde gos ors"
  end

  test "long string" do
    msg = "If man was meant to stay on the ground, god would have given us roots."
    cipher = "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn sseoau"
    assert CryptoSquare.encode(msg) == cipher
  end
end
