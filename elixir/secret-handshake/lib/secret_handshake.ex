defmodule SecretHandshake do
  use Bitwise

  @actions [
    {List, :insert_at, [0, "wink"]},
    {List, :insert_at, [0, "double blink"]},
    {List, :insert_at, [0, "close your eyes"]},
    {List, :insert_at, [0, "jump"]},
    {Enum, :reverse, []}
  ]

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(0), do: []

  def commands(code) do
    @actions
    |> Enum.with_index()
    |> Enum.reduce([], &perform_secret_handshake(code, &1, &2))
    |> Enum.reverse()
  end

  def perform_secret_handshake(code, {{mod, fun, args}, index}, acc) do
    if set?(code, index) do
      apply(mod, fun, [acc | args])
    else
      acc
    end
  end

  def set?(code, index) do
    (code &&& mask(index)) > 0
  end

  def mask(number) do
    1 <<< number
  end
end
