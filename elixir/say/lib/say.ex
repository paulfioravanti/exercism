defmodule Say do
  @number_words %{
    1 => "one",
    2 => "two",
    3 => "three",
    4 => "four",
    5 => "five",
    6 => "six",
    7 => "seven",
    8 => "eight",
    9 => "nine",
    10 => "ten",
    11 => "eleven",
    12 => "twelve",
    13 => "thirteen",
    14 => "fourteen",
    15 => "fifteen",
    16 => "sixteen",
    17 => "seventeen",
    18 => "eighteen",
    19 => "nineteen",
    20 => "twenty",
    30 => "thirty",
    40 => "forty",
    50 => "fifty",
    60 => "sixty",
    70 => "seventy",
    80 => "eighty",
    90 => "ninety"
  }

  @thousand 3
  @million 6
  @billion 9

  @scales %{
    2 => "hundred",
    @thousand => "thousand",
    @million => "million",
    @billion => "billion"
  }

  defguardp out_of_range?(number) when number < 0 or number > 999_999_999_999
  defguardp up_to_twenty?(number) when number < 21
  defguardp up_to_ninety_nine?(number) when number < 100

  defguardp ten_thousand_up_to_one_million?(number)
            when number > 9_999 and number < 1_000_000

  defguardp ten_million_up_to_one_billion?(number)
            when number > 9_999_999 and number < 1_000_000_000

  defguardp ten_billion_up_to_one_trillion?(number)
            when number > 9_999_999_999 and number < 1_000_000_000_000

  @doc """
  Translate a positive integer into English.
  """
  @spec in_english(integer) :: {atom, String.t()}
  def in_english(number) when out_of_range?(number) do
    {:error, "number is out of range"}
  end

  def in_english(0), do: {:ok, "zero"}

  def in_english(number) when up_to_twenty?(number) do
    {:ok, @number_words[number]}
  end

  def in_english(number) when up_to_ninety_nine?(number) do
    {:ok, hypenated_word(number)}
  end

  def in_english(number) do
    {:ok, full_word(number)}
  end

  defp hypenated_word(number) do
    [_tens, ones] = Integer.digits(number)
    @number_words[number - ones] <> "-" <> @number_words[ones]
  end

  defp full_word(0), do: ""

  defp full_word(number) when up_to_twenty?(number) do
    @number_words[number]
  end

  defp full_word(number) when up_to_ninety_nine?(number) do
    hypenated_word(number)
  end

  defp full_word(number) when ten_thousand_up_to_one_million?(number) do
    number
    |> split_list_by_scale(@thousand)
  end

  defp full_word(number) when ten_million_up_to_one_billion?(number) do
    number
    |> split_list_by_scale(@million)
  end

  defp full_word(number) when ten_billion_up_to_one_trillion?(number) do
    number
    |> split_list_by_scale(@billion)
  end

  defp full_word(number) do
    [head | tail] = Integer.digits(number)

    head_word = @number_words[head]

    scale =
      tail
      |> length()
      |> (fn length -> @scales[length] end).()

    tail_words =
      tail
      |> Integer.undigits()
      |> full_word()
      |> format_tail_word()

    head_word <> " " <> scale <> tail_words
  end

  defp format_tail_word(""), do: ""
  defp format_tail_word(word), do: " " <> word

  defp split_list_by_scale(number, split) do
    {head_scale_list, rest_list} =
      number
      |> Integer.digits()
      |> Enum.split(-split)

    scale_number = Integer.undigits(head_scale_list)
    rest_number = Integer.undigits(rest_list)

    full_word(scale_number) <>
      " " <>
      @scales[split] <>
      " " <>
      full_word(rest_number)
  end
end
