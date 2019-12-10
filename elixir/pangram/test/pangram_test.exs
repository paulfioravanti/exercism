defmodule PangramTest do
  use ExUnit.Case

  test "empty sentence" do
    refute Pangram.pangram?("")
  end

  test "pangram with only lower case" do
    assert Pangram.pangram?("the quick brown fox jumps over the lazy dog")
  end

  test "missing character 'x'" do
    refute Pangram.pangram?(
             "a quick movement of the enemy will jeopardize five gunboats"
           )
  end

  test "another missing character 'x'" do
    refute Pangram.pangram?("the quick brown fish jumps over the lazy dog")
  end

  test "pangram with underscores" do
    assert Pangram.pangram?("the_quick_brown_fox_jumps_over_the_lazy_dog")
  end

  test "pangram with numbers" do
    assert Pangram.pangram?("the 1 quick brown fox jumps over the 2 lazy dogs")
  end

  test "missing letters replaced by numbers" do
    refute Pangram.pangram?("7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog")
  end

  test "pangram with mixed case and punctuation" do
    assert Pangram.pangram?("Five quacking Zephyrs jolt my wax bed.")
  end

  test "pangram with non ascii characters" do
    assert Pangram.pangram?(
             "Victor jagt zwölf Boxkämpfer quer über den großen Sylter Deich."
           )
  end

  test "pangram in alphabet other than ASCII" do
    refute Pangram.pangram?(
             "Широкая электрификация южных губерний даст мощный толчок подъёму сельского хозяйства."
           )
  end
end
