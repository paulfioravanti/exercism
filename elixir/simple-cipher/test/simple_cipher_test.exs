defmodule SimpleCipherTest do
  use ExUnit.Case

  test "encoding with 'a' returns the original text" do
    assert SimpleCipher.encode("a", "a") == "a"
    assert SimpleCipher.encode("b", "a") == "b"
    assert SimpleCipher.encode("c", "a") == "c"
  end

  test "encoding with another key returns shifted text" do
    assert SimpleCipher.encode("a", "d") == "d"
    assert SimpleCipher.encode("b", "d") == "e"
    assert SimpleCipher.encode("c", "d") == "f"
  end

  test "decoding with 'a' returns the original text" do
    assert SimpleCipher.decode("a", "a") == "a"
    assert SimpleCipher.decode("b", "a") == "b"
    assert SimpleCipher.decode("c", "a") == "c"
  end

  test "decoding with another key returns unshifted text" do
    assert SimpleCipher.decode("d", "d") == "a"
    assert SimpleCipher.decode("e", "d") == "b"
    assert SimpleCipher.decode("f", "d") == "c"
  end

  test "key uses per-letter translation for encoding" do
    key = "abc"

    assert SimpleCipher.encode("abc", key) == "ace"
    assert SimpleCipher.encode("bcd", key) == "bdf"
    assert SimpleCipher.encode("cde", key) == "ceg"
    assert SimpleCipher.encode("iamapandabear", "dddddddddddddd") == "ldpdsdqgdehdu"
  end

  test "key uses per-letter translation for decoding" do
    key = "abc"

    assert SimpleCipher.decode("ace", key) == "abc"
    assert SimpleCipher.decode("bdf", key) == "bcd"
    assert SimpleCipher.decode("ceg", key) == "cde"
    assert SimpleCipher.decode("ldpdsdqgdehdu", "dddddddddddddd") == "iamapandabear"
  end

  test "only lowercase a-z are translated, rest are passed through" do
    assert SimpleCipher.encode("this is a test!", "d") == "wklv lv d whvw!"
    assert SimpleCipher.decode("wklv lv d whvw!", "d") == "this is a test!"
  end

  test "if key is shorter than text, repeat key" do
    assert SimpleCipher.encode("abc", "a") == "abc"
    assert SimpleCipher.encode("abcdefghi", "abc") == "acedfhgik"
  end

  test "if key is longer than text, only use as much as needed" do
    key = "somewhatlongkey"

    assert SimpleCipher.encode("abc", key) == "spo"
    assert SimpleCipher.decode("abc", key) == "inq"
  end

  test "if you know both the encoded and decoded text, you can figure out the key" do
    key = "supersecretkey"

    plaintext = "attackxatxdawn"
    ciphertext = SimpleCipher.encode(plaintext, key)

    assert SimpleCipher.decode(ciphertext, plaintext) == key
  end
end
