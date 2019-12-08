defmodule Forth do
  defmodule Evaluator do
    alias Forth.{DivisionByZero, InvalidWord, StackUnderflow, UnknownWord}

    @words %{
      "+" => &Evaluator.add/1,
      "-" => &Evaluator.sub/1,
      "*" => &Evaluator.mult/1,
      "/" => &Evaluator.div/1,
      "DUP" => &Evaluator.dup/1,
      "DROP" => &Evaluator.drop/1,
      "SWAP" => &Evaluator.swap/1,
      "OVER" => &Evaluator.over/1
    }
    @final_element -1

    defstruct stack: [], words: @words

    @type t :: %Evaluator{stack: List.t(), words: Map.t()}

    def new, do: %Evaluator{}

    def parse_term(term) do
      case Integer.parse(term) do
        {int, ""} ->
          int

        :error ->
          String.upcase(term)
      end
    end

    def eval(%Evaluator{stack: stack} = evaluator, value)
        when is_integer(value) do
      %Evaluator{evaluator | stack: [value | stack]}
    end

    def eval(%Evaluator{words: words} = evaluator, ": " <> custom_word) do
      [custom_word | definition] = parse_custom_word(custom_word)
      implementation = Enum.map(definition, &fetch_implementation(words, &1))
      words = Map.put(words, custom_word, implementation)
      %Evaluator{evaluator | words: words}
    end

    def eval(%Evaluator{words: words} = evaluator, word) do
      words
      |> Map.fetch!(word)
      |> eval_definition(evaluator)
    rescue
      KeyError ->
        reraise UnknownWord, __STACKTRACE__
    end

    def add(%Evaluator{stack: [head, next | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [next + head | tail]}
    end

    def sub(%Evaluator{stack: [head, next | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [next - head | tail]}
    end

    def mult(%Evaluator{stack: [head, next | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [next * head | tail]}
    end

    def div(%Evaluator{stack: [0 | _tail]}), do: raise(DivisionByZero)

    def div(%Evaluator{stack: [head, next | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [div(next, head) | tail]}
    end

    def dup(%Evaluator{stack: []}), do: raise(StackUnderflow)

    def dup(%Evaluator{stack: [head | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [head, head | tail]}
    end

    def drop(%Evaluator{stack: []}), do: raise(StackUnderflow)

    def drop(%Evaluator{stack: [_head | tail]} = evaluator) do
      %Evaluator{evaluator | stack: tail}
    end

    def swap(%Evaluator{stack: []}), do: raise(StackUnderflow)
    def swap(%Evaluator{stack: [_head]}), do: raise(StackUnderflow)

    def swap(%Evaluator{stack: [head, next | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [next, head | tail]}
    end

    def over(%Evaluator{stack: []}), do: raise(StackUnderflow)
    def over(%Evaluator{stack: [_head]}), do: raise(StackUnderflow)

    def over(%Evaluator{stack: [head, next | tail]} = evaluator) do
      %Evaluator{evaluator | stack: [next, head, next | tail]}
    end

    def format_stack(%Evaluator{stack: []}), do: ""

    def format_stack(%Evaluator{stack: stack}) do
      stack
      |> Enum.reverse()
      |> Enum.join(" ")
    end

    defp eval_definition(definition, evaluator) when is_function(definition) do
      apply(definition, [evaluator])
    end

    defp eval_definition(definition, evaluator) when is_list(definition) do
      Enum.reduce(definition, evaluator, &apply(&1, [&2]))
    end

    defp parse_custom_word(custom_word) do
      custom_word
      |> String.split()
      |> Enum.drop(@final_element)
      |> parse_custom_word_name()
    end

    def parse_custom_word_name([word | _definition] = custom_word) do
      case Integer.parse(word) do
        # Attempt to re-define a number
        {_int, ""} ->
          raise InvalidWord

        # Happy path
        :error ->
          custom_word
      end
    end

    defp fetch_implementation(words, definition) do
      case Map.fetch(words, definition) do
        {:ok, implementation} ->
          implementation

        :error ->
          # create on-the-fly custom implementation
          fn evaluator ->
            eval(evaluator, parse_term(definition))
          end
      end
    end
  end

  @words ~r/:.+;|[[:graph:]]+/u

  @opaque evaluator :: Evaluator.t()

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
    |> Enum.map(&Evaluator.parse_term/1)
    |> Enum.reduce(ev, &Evaluator.eval(&2, &1))
  end

  @doc """
  Return the current stack as a string with the element on top of the stack
  being the rightmost element in the string.
  """
  @spec format_stack(evaluator) :: String.t()
  def format_stack(ev), do: Evaluator.format_stack(ev)

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
