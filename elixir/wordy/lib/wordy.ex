defmodule Wordy do
  @math_question ~r/
    \A
    What\sis\s                           # What is
    (?<initial_value>-?\d+)\s            # 1, -1 etc
    (?<messages>(\w+(\sby)?\s-?\d+\s?)+) # plus 1, multiplied by 1 plus 2 etc
    \?                                   # question mark at the end
    \z
  /x

  @messages %{
    "plus" => &+/2,
    "minus" => &-/2,
    "multiplied" => &*/2,
    "divided" => &//2
  }

  @doc """
  Calculate the math problem in the sentence.
  """
  @spec answer(String.t()) :: integer
  def answer(question) do
    case Regex.named_captures(@math_question, question) do
      nil ->
        raise ArgumentError

      %{"initial_value" => initial_value, "messages" => messages} ->
        initial_value = String.to_integer(initial_value)

        messages
        |> parse_messages()
        |> Enum.reduce(initial_value, &send_message/2)
    end
  end

  defp parse_messages(messages) do
    messages
    |> String.replace("by ", "")
    |> String.split()
    |> Enum.chunk_every(2)
    |> Enum.map(&message_value_pair/1)
  end

  def message_value_pair([message, value]) do
    {@messages[message], String.to_integer(value)}
  end

  defp send_message({message, value}, acc), do: apply(message, [acc, value])
end
