defmodule BobTest do
  use ExUnit.Case

  test "stating something" do
    assert Bob.hey("Tom-ay-to, tom-aaaah-to.") == "Whatever."
  end

  test "shouting" do
    assert Bob.hey("WATCH OUT!") == "Whoa, chill out!"
  end

  test "shouting gibberish" do
    assert Bob.hey("FCECDFCAAB") == "Whoa, chill out!"
  end

  test "asking a question" do
    assert Bob.hey("Does this cryogenic chamber make me look fat?") == "Sure."
  end

  test "asking a numeric question" do
    assert Bob.hey("You are, what, like 15?") == "Sure."
  end

  test "asking gibberish" do
    assert Bob.hey("fffbbcbeab?") == "Sure."
  end

  test "talking forcefully" do
    assert Bob.hey("Let's go make out behind the gym!") == "Whatever."
  end

  test "using acronyms in regular speech" do
    assert Bob.hey("It's OK if you don't want to go to the DMV.") == "Whatever."
  end

  test "talking in capitals" do
    assert Bob.hey("This Isn't Shouting!") == "Whatever."
  end

  test "forceful question" do
    assert Bob.hey("WHAT THE HELL WERE YOU THINKING?") ==
             "Calm down, I know what I'm doing!"
  end

  test "asking in capitals" do
    assert Bob.hey("THIS ISN'T SHOUTING?") ==
             "Calm down, I know what I'm doing!"
  end

  test "shouting numbers" do
    assert Bob.hey("1, 2, 3 GO!") == "Whoa, chill out!"
  end

  test "shouting with special characters" do
    assert Bob.hey("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!") ==
             "Whoa, chill out!"
  end

  test "shouting with no exclamation mark" do
    assert Bob.hey("I HATE THE DMV") == "Whoa, chill out!"
  end

  test "statement containing question mark" do
    assert Bob.hey("Ending with ? means a question.") == "Whatever."
  end

  test "silence" do
    assert Bob.hey("") == "Fine. Be that way!"
  end

  test "prolonged silence" do
    assert Bob.hey("  ") == "Fine. Be that way!"
  end

  test "alternate silence" do
    assert Bob.hey("\t\t\t\t\t\t\t\t\t\t") == "Fine. Be that way!"
  end

  test "only numbers" do
    assert Bob.hey("1, 2, 3") == "Whatever."
  end

  test "multiple line question" do
    assert Bob.hey("\nDoes this cryogenic chamber make me look fat?\nNo.") ==
             "Whatever."
  end

  test "question with numbers" do
    assert Bob.hey("4?") == "Sure."
  end

  test "non-letters with question" do
    assert Bob.hey(":) ?") == "Sure."
  end

  test "prattling on" do
    assert Bob.hey("Wait! Hang on. Are you going to be OK?") == "Sure."
  end

  test "starting with whitespace" do
    assert Bob.hey("         hmmmmmmm...") == "Whatever."
  end

  test "ending with whitespace" do
    assert Bob.hey("Okay if like my  spacebar  quite a bit?   ") == "Sure."
  end

  test "other whitespace" do
    assert Bob.hey("\n\r \t") == "Fine. Be that way!"
  end

  test "non-question ending with whitespace" do
    assert Bob.hey("This is a statement ending with whitespace     ") ==
             "Whatever."
  end

  test "shouting in Russian" do
    assert Bob.hey("УХОДИ") == "Whoa, chill out!"
  end
end
