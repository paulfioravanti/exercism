defmodule AnagramTest do
  use ExUnit.Case

  test "no matches" do
    matches = Anagram.match("diaper", ~w(hello world zombies pants))
    assert matches == []
  end

  test "detects two anagrams" do
    matches = Anagram.match("master", ~w(stream pigeon maters))
    assert matches == ~w(stream maters)
  end

  test "does not detect anagram subsets" do
    matches = Anagram.match("good", ~w(dog goody))
    assert matches == []
  end

  test "detects anagram" do
    matches = Anagram.match("listen", ~w(enlists google inlets banana))
    assert matches == ~w(inlets)
  end

  test "detects three anagrams" do
    matches =
      Anagram.match(
        "allergy",
        ~w(gallery ballerina regally clergy largely leading)
      )

    assert matches == ~w(gallery regally largely)
  end

  test "detects multiple anagrams with different case" do
    matches = Anagram.match("nose", ~w(Eons ONES))
    assert matches == ~w(Eons ONES)
  end

  test "does not detect non-anagrams with identical checksum" do
    matches = Anagram.match("mass", ~w(last))
    assert matches == []
  end

  test "detect anagramss case-insensitively" do
    matches = Anagram.match("orchestra", ~w(cashregister Carthorse radishes))
    assert matches == ~w(Carthorse)
  end

  test "detects anagrams using case-insensitive subject" do
    matches = Anagram.match("Orchestra", ~w(cashregister carthorse radishes))
    assert matches == ~w(carthorse)
  end

  test "detects anagrams using case-insensitive possible matches" do
    matches = Anagram.match("orchestra", ~w(cashregister Carthorse radishes))
    assert matches == ~w(Carthorse)
  end

  test "does not detect an anagram if the original word is repeated" do
    matches = Anagram.match("go", ~w(go Go GO))
    assert matches == []
  end

  test "anagrams must use all letters exactly once" do
    matches = Anagram.match("tapper", ~w(patter))
    assert matches == []
  end

  test "words are not anagrams of themselves (case-insensitive)" do
    matches = Anagram.match("BANANA", ~w(BANANA Banana banana))
    assert matches == []
  end

  test "words other than themselves can be anagrams" do
    matches = Anagram.match("LISTEN", ~w(Listen Silent LISTEN))
    assert matches == ~w(Silent)
  end
end
