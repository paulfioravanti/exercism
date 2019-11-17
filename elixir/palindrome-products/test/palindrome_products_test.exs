defmodule PalindromeProductsTest do
  use ExUnit.Case

  test "largest palindrome from single digit factors" do
    palindromes = Palindromes.generate(9)
    assert palindromes |> Map.keys() |> Enum.sort() |> List.last() == 9
    assert Enum.sort(palindromes[9]) == [[1, 9], [3, 3]]
  end

  test "largest palindrome from double digit factors" do
    palindromes = Palindromes.generate(99, 10)
    assert palindromes |> Map.keys() |> Enum.sort() |> List.last() == 9009
    assert palindromes[9009] == [[91, 99]]
  end

  test "smallest palindrome from double digit factors" do
    palindromes = Palindromes.generate(99, 10)
    assert palindromes |> Map.keys() |> Enum.sort() |> hd == 121
    assert palindromes[121] == [[11, 11]]
  end

  test "largest palindrome from triple digit factors" do
    palindromes = Palindromes.generate(999, 100)
    assert palindromes |> Map.keys() |> Enum.sort() |> List.last() == 906_609
    assert palindromes[906_609] == [[913, 993]]
  end

  test "smallest palindromes from triple digit factors" do
    palindromes = Palindromes.generate(999, 100)
    assert palindromes |> Map.keys() |> Enum.sort() |> hd == 10201
    assert palindromes[10201] == [[101, 101]]
  end
end
