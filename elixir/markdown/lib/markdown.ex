defmodule Markdown do
  @header_attribute "#"
  @list_attribute "*"
  @emphasis_attribute "_"
  @strong_attribute String.duplicate(@emphasis_attribute, 2)
  @open_list_item_tag "<li>"
  @close_list_item_tag "</li>"
  @open_ul_tag "<ul>"
  @close_ul_tag "</ul>"
  @open_strong_tag "<strong>"
  @close_strong_tag "</strong>"
  @open_emphasis_tag "<em>"
  @close_emphasis_tag "</em>"
  @open_paragraph_tag "<p>"
  @close_paragraph_tag "</p>"

  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    ## Examples

    iex> Markdown.parse("This is a paragraph")
    "<p>This is a paragraph</p>"

    iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
    "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"
  """
  @spec parse(String.t()) :: String.t()
  def parse(markdown) do
    markdown
    |> String.split("\n")
    |> process([])
    |> Enum.reverse()
    |> Enum.join()
  end

  defp process([], acc), do: acc

  defp process([@header_attribute <> _rest = head | tail], acc) do
    header =
      head
      |> parse_header()
      |> enclose_with_header_tag()

    process(tail, [header | acc])
  end

  defp process([@list_attribute <> _rest | _tail] = lines, acc) do
    {tail, list} = parse_list(lines, [@open_ul_tag])
    process(tail, [list | acc])
  end

  defp process([head | tail], acc) do
    paragraph =
      head
      |> String.split()
      |> join_words_with_tags()
      |> enclose_with_paragraph_tag()

    process(tail, [paragraph | acc])
  end

  defp parse_header(header) do
    [formatting, text] = String.split(header, " ", parts: 2)
    level = String.length(formatting)
    {level, text}
  end

  defp parse_list([@list_attribute <> rest | tail], acc) do
    list_item =
      rest
      |> String.split()
      |> join_words_with_tags()
      |> enclose_with_list_item_tag()

    parse_list(tail, [list_item | acc])
  end

  defp parse_list(lines, acc) do
    list =
      acc
      |> Enum.reverse()
      |> Enum.join()
      |> Kernel.<>(@close_ul_tag)

    {lines, list}
  end

  defp join_words_with_tags(words) do
    words
    |> Enum.map(&replace_md_with_tag/1)
    |> Enum.join(" ")
  end

  defp replace_md_with_tag(word) do
    word
    |> replace_prefix()
    |> replace_suffix()
  end

  defp replace_prefix(@strong_attribute <> rest), do: @open_strong_tag <> rest
  defp replace_prefix(@emphasis_attribute <> rest), do: @open_emphasis_tag <> rest
  defp replace_prefix(word), do: word

  defp replace_suffix(word) do
    cond do
      String.ends_with?(word, @strong_attribute) ->
        String.replace(word, @strong_attribute, @close_strong_tag)

      String.ends_with?(word, @emphasis_attribute) ->
        String.replace(word, @emphasis_attribute, @close_emphasis_tag)

      true ->
        word
    end
  end

  defp enclose_with_header_tag({level, header}) do
    "<h#{level}>" <> header <> "</h#{level}>"
  end

  defp enclose_with_list_item_tag(list_item) do
    @open_list_item_tag <> list_item <> @close_list_item_tag
  end

  defp enclose_with_paragraph_tag(paragraph) do
    @open_paragraph_tag <> paragraph <> @close_paragraph_tag
  end
end
