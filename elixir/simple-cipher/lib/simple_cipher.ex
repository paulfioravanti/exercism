defmodule SimpleCipher do
  @a_ordinal ?a
  @alphabet Enum.to_list(?a..?z)
  @alphabet_length length(@alphabet)

  defguardp non_lower_alpha?(text_char) when not (text_char in @alphabet)

  @doc """
  Given a `plaintext` and `key`, encode each character of the `plaintext` by
  shifting it by the corresponding letter in the alphabet shifted by the number
  of letters represented by the `key` character, repeating the `key` if it is
  shorter than the `plaintext`.

  For example, for the letter 'd', the alphabet is rotated to become:

  defghijklmnopqrstuvwxyzabc

  You would encode the `plaintext` by taking the current letter and mapping it
  to the letter in the same position in this rotated alphabet.

  abcdefghijklmnopqrstuvwxyz
  defghijklmnopqrstuvwxyzabc

  "a" becomes "d", "t" becomes "w", etc...

  Each letter in the `plaintext` will be encoded with the alphabet of the `key`
  character in the same position. If the `key` is shorter than the `plaintext`,
  repeat the `key`.

  Example:

  plaintext = "testing"
  key = "abc"

  The key should repeat to become the same length as the text, becoming
  "abcabca". If the key is longer than the text, only use as many letters of it
  as are necessary.
  """
  def encode(plaintext, key) do
    shift(plaintext, key, &+/2)
  end

  @doc """
  Given a `ciphertext` and `key`, decode each character of the `ciphertext` by
  finding the corresponding letter in the alphabet shifted by the number of
  letters represented by the `key` character, repeating the `key` if it is
  shorter than the `ciphertext`.

  The same rules for key length and shifted alphabets apply as in `encode/2`,
  but you will go the opposite way, so "d" becomes "a", "w" becomes "t",
  etc..., depending on how much you shift the alphabet.
  """
  def decode(ciphertext, key) do
    shift(ciphertext, key, &-/2)
  end

  defp shift(text, key, fun) do
    text
    |> String.to_charlist()
    |> Enum.zip(cycle_key(key))
    |> Enum.map_join("", &substitute(fun, &1))
  end

  defp cycle_key(key) do
    key
    |> String.to_charlist()
    |> Stream.cycle()
  end

  defp substitute(_fun, {text_char, _key_char})
       when non_lower_alpha?(text_char) do
    <<text_char::utf8>>
  end

  defp substitute(fun, {text_char, key_char}) do
    value =
      fun
      |> apply([text_char - @a_ordinal, key_char - @a_ordinal])
      |> Kernel.+(@alphabet_length)
      |> Integer.mod(@alphabet_length)
      |> Kernel.+(@a_ordinal)

    <<value::utf8>>
  end
end
