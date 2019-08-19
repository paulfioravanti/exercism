defmodule PigLatin do
  @suffix "ay"
  @vowels "aeiou"
  @vowel_chars String.codepoints(@vowels)
  @xy_vowel_sound ~r/\A(x|y)[^#{@vowels}]/
  @consonant_sound ~r/
    \A
    (?<consonant_sound>
      (?:.qu|qu|.)[^#{@vowels}y]*
    )
    (?<rest>
      .*
    )
  /x

  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.

  Words beginning with consonants should have the consonant moved to the end of
  the word, followed by "ay".

  Words beginning with vowels (aeiou) should have "ay" added to the end of the
  word.

  Some groups of letters are treated like consonants, including "ch", "qu",
  "squ", "th", "thr", and "sch".

  Some groups are treated like vowels, including "yt" and "xr".
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split(" ")
    |> Enum.map_join(" ", &translate_word/1)
  end

  defp translate_word(word) do
    if starts_with_vowel_sound?(word) do
      word <> @suffix
    else
      %{"consonant_sound" => consonant_sound, "rest" => rest} =
        Regex.named_captures(@consonant_sound, word)

      rest <> consonant_sound <> @suffix
    end
  end

  defp starts_with_vowel_sound?(word) do
    String.starts_with?(word, @vowel_chars) or
      Regex.match?(@xy_vowel_sound, word)
  end
end
