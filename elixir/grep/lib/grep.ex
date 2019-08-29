defmodule Grep do
  @entire_lines_only "-x"
  @caseless "i"
  @ignore_case "-" <> @caseless
  @invert_pattern "-v"
  @starting_index 1
  @filenames_only "-l"
  @line_numbers "-n"

  @spec grep(String.t(), [String.t()], [String.t()]) :: String.t()
  def grep(pattern, flags, files) do
    regex = generate_regex(pattern, flags)
    multiple_files = length(files) > 1
    line_guard = generate_line_guard(flags)
    params = {flags, regex, multiple_files, line_guard}

    files
    |> Enum.reduce([], &grep_file(params, &1, &2))
    |> Enum.reverse()
    |> Enum.join()
  end

  defp generate_regex(pattern, flags) do
    source =
      if Enum.member?(flags, @entire_lines_only) do
        "\\A#{pattern}\n\\z"
      else
        pattern
      end

    options = if Enum.member?(flags, @ignore_case), do: @caseless, else: ""
    Regex.compile!(source, options)
  end

  defp generate_line_guard(flags) do
    if Enum.member?(flags, @invert_pattern) do
      fn line, regex, acc ->
        if String.match?(line, regex), do: throw({:next, acc})
      end
    else
      fn line, regex, acc ->
        unless String.match?(line, regex), do: throw({:next, acc})
      end
    end
  end

  defp grep_file(params, filename, acc) do
    params = Tuple.append(params, filename)

    filename
    |> File.stream!()
    |> Stream.with_index(@starting_index)
    |> Enum.reduce(acc, &grep_line(params, &1, &2))
  end

  defp grep_line(params, {line, index}, acc) do
    {flags, regex, multiple_files, line_guard, filename} = params
    line_guard.(line, regex, acc)

    if Enum.member?(flags, @filenames_only) do
      add_filename_only(filename, acc)
    else
      add_line(flags, multiple_files, filename, index, line, acc)
    end
  catch
    {:next, acc} ->
      acc
  end

  defp add_filename_only(filename, acc) do
    filename = filename <> "\n"
    if Enum.member?(acc, filename), do: acc, else: [filename | acc]
  end

  defp add_line(flags, multiple_files, filename, index, line, acc) do
    header =
      ""
      |> maybe_add_filename(filename, multiple_files)
      |> maybe_add_line_number(flags, index)

    [header <> line | acc]
  end

  defp maybe_add_filename(string, filename, true), do: string <> filename <> ":"
  defp maybe_add_filename(string, _filename, false), do: string

  defp maybe_add_line_number(string, flags, index) do
    if Enum.member?(flags, @line_numbers) do
      string <> to_string(index) <> ":"
    else
      string
    end
  end
end
