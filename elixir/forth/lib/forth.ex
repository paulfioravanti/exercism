defmodule Forth do
  defmodule Evaluator do
    @arithmetic_words %{
      "+" => &Kernel.+/2,
      "-" => &Kernel.-/2,
      "*" => &Kernel.*/2,
      "/" => &Kernel.div/2
    }
    @arithmetic_operations Map.keys(@arithmetic_words)

    defguardp arithmetic_operation?(word, first, second)
              when word in @arithmetic_operations and
                     is_integer(first) and
                     is_integer(second)

    defstruct stack: [],
              words: @arithmetic_words

    def new, do: %Evaluator{}

    def eval(
          %Evaluator{stack: [second, first | tail], words: words} = evaluator,
          word
        )
        when arithmetic_operation?(word, first, second) do
      result = apply(words[word], [first, second])
      %Evaluator{evaluator | stack: [result | tail]}
    rescue
      ArithmeticError ->
        reraise Forth.DivisionByZero, __STACKTRACE__
    end

    def eval(%Evaluator{stack: []}, "DUP"), do: raise(Forth.StackUnderflow)

    def eval(%Evaluator{stack: [head | tail]} = evaluator, "DUP") do
      %Evaluator{evaluator | stack: [head, head | tail]}
    end

    def eval(%Evaluator{stack: []}, "DROP"), do: raise(Forth.StackUnderflow)

    def eval(%Evaluator{stack: [_head | tail]} = evaluator, "DROP") do
      %Evaluator{evaluator | stack: tail}
    end

    def eval(%Evaluator{stack: []}, "SWAP"), do: raise(Forth.StackUnderflow)
    def eval(%Evaluator{stack: [_]}, "SWAP"), do: raise(Forth.StackUnderflow)

    def eval(%Evaluator{stack: [second, first | tail]} = evaluator, "SWAP") do
      %Evaluator{evaluator | stack: [first, second | tail]}
    end

    def eval(%Evaluator{stack: []}, "OVER"), do: raise(Forth.StackUnderflow)
    def eval(%Evaluator{stack: [_]}, "OVER"), do: raise(Forth.StackUnderflow)

    def eval(%Evaluator{stack: [second, first | tail]} = evaluator, "OVER") do
      %Evaluator{evaluator | stack: [first, second, first | tail]}
    end

    def eval(%Evaluator{stack: stack} = evaluator, value) do
      %Evaluator{evaluator | stack: [value | stack]}
    end

    def format_stack(%Evaluator{stack: []}), do: ""

    def format_stack(%Evaluator{stack: stack}) do
      stack
      |> Enum.reverse()
      |> Enum.join(" ")
    end
  end

  @opaque evaluator :: Evaluator.t()

  @non_word_characters ~r/[[:space:]|[:cntrl:]]+/u

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
    s
    |> String.split(@non_word_characters)
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
    String.to_integer(term)
  rescue
    ArgumentError ->
      String.upcase(term)
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
