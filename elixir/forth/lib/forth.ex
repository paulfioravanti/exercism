defmodule Forth do
  defmodule Evaluator do
    @words %{
      "+" => [&Evaluator.add/1],
      "-" => [&Evaluator.sub/1],
      "*" => [&Evaluator.mult/1],
      "/" => [&Evaluator.div/1],
      "DUP" => [&Evaluator.dup/1],
      "DROP" => [&Evaluator.drop/1],
      "SWAP" => [&Evaluator.swap/1],
      "OVER" => [&Evaluator.over/1]
    }
    @final_element -1

    defstruct stack: [], words: @words

    def new, do: %Evaluator{}

    def eval(%Evaluator{stack: stack} = evaluator, value)
        when is_integer(value) do
      %Evaluator{evaluator | stack: [value | stack]}
    end

    def eval(%Evaluator{words: words} = evaluator, ": " <> word) do
      [word | definition] = parse_custom_word(word)
      word = parse(word)
      implementation = Enum.map(definition, &fetch_implementation(words, &1))
      words = Map.put(words, word, implementation)
      %Evaluator{evaluator | words: words}
    end

    def eval(%Evaluator{words: words} = evaluator, value) do
      fun = Map.fetch!(words, value)
      Enum.reduce(fun, evaluator, &apply(&1, [&2]))
    rescue
      KeyError ->
        reraise Forth.UnknownWord, __STACKTRACE__
    end

    def add(%Evaluator{stack: [second, first | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [first + second | tail]}
    end

    def sub(%Evaluator{stack: [second, first | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [first - second | tail]}
    end

    def mult(%Evaluator{stack: [second, first | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [first * second | tail]}
    end

    def div(%Evaluator{stack: [second, first | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [div(first, second) | tail]}
    rescue
      ArithmeticError ->
        reraise Forth.DivisionByZero, __STACKTRACE__
    end

    def dup(%Evaluator{stack: []}), do: raise(Forth.StackUnderflow)

    def dup(%Evaluator{stack: [head | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [head, head | tail]}
    end

    def drop(%Evaluator{stack: []}), do: raise(Forth.StackUnderflow)

    def drop(%Evaluator{stack: [_head | tail]} = evaluator) do
      %Evaluator{evaluator | stack: tail}
    end

    def swap(%Evaluator{stack: []}), do: raise(Forth.StackUnderflow)
    def swap(%Evaluator{stack: [_value]}), do: raise(Forth.StackUnderflow)

    def swap(%Evaluator{stack: [second, first | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [first, second | tail]}
    end

    def over(%Evaluator{stack: []}), do: raise(Forth.StackUnderflow)
    def over(%Evaluator{stack: [_value]}), do: raise(Forth.StackUnderflow)

    def over(%Evaluator{stack: [second, first | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [first, second, first | tail]}
    end

    def format_stack(%Evaluator{stack: []}), do: ""

    def format_stack(%Evaluator{stack: stack}) do
      stack
      |> Enum.reverse()
      |> Enum.join(" ")
    end

    defp parse_custom_word(word) do
      word
      |> String.split()
      |> Enum.drop(@final_element)
    end

    defp fetch_implementation(words, fun) do
      words
      |> Map.fetch!(fun)
      |> hd()
    end

    defp parse(word) do
      case Integer.parse(word) do
        {_int, ""} ->
          raise Forth.InvalidWord

        :error ->
          word
      end
    end
  end

  @opaque evaluator :: Evaluator.t()

  @words ~r/:.+;|[[:graph:]]+/u

  @doc """
  Create a new evaluator.
  """
  @spec new() :: evaluator
  def new, do: Evaluator.new()

  @doc """
  Evaluate an input string, updating the evaluator state.
  """
  @spec eval(evaluator, String.t()) :: evaluator
  def eval(ev, s) do
    @words
    |> Regex.scan(s)
    |> List.flatten()
    |> Enum.map(&parse/1)
    |> Enum.reduce(ev, &Evaluator.eval(&2, &1))
  end

  @doc """
  Return the current stack as a string with the element on top of the stack
  being the rightmost element in the string.
  """
  @spec format_stack(evaluator) :: String.t()
  def format_stack(ev) do
    Evaluator.format_stack(ev)
  end

  defp parse(term) do
    case Integer.parse(term) do
      {int, ""} ->
        int

      :error ->
        String.upcase(term)
    end
  end

  defmodule StackUnderflow do
    defexception []
    def message(_), do: "stack underflow"
  end

  defmodule InvalidWord do
    defexception word: nil
    def message(e), do: "invalid word: #{inspect(e.word)}"
  end

  defmodule UnknownWord do
    defexception word: nil
    def message(e), do: "unknown word: #{inspect(e.word)}"
  end

  defmodule DivisionByZero do
    defexception []
    def message(_), do: "division by zero"
  end
end
