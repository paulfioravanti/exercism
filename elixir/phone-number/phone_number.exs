defmodule Phone do
  @valid_number ~r/\A
                 \+?1?                 # optional country code
                 \s*                   # optional divider
                 \(?                   # optional left parenthesis
                 (?<area_code>
                   [2-9]\d{2}          # area code
                 )
                 \)?                   # optional right parenthesis
                 [-\.\s]*              # optional divider
                 (?<exchange_code>
                   [2-9]\d{2}          # exchange code
                 )
                 [-\.\s]*              # optional divider
                 (?<subscriber_number>
                   \d{4}               # subscriber number
                 )
                \z/x

  @doc """
  Remove formatting from a phone number.

  Returns "0000000000" if phone number is not valid
  (10 digits or "1" followed by 10 digits)

  ## Examples

  iex> Phone.number("212-555-0100")
  "2125550100"

  iex> Phone.number("+1 (212) 555-0100")
  "2125550100"

  iex> Phone.number("+1 (212) 055-0100")
  "0000000000"

  iex> Phone.number("(212) 555-0100")
  "2125550100"

  iex> Phone.number("867.5309")
  "0000000000"
  """
  @spec number(String.t()) :: String.t()
  def number(raw) do
    case phone_number_parts(raw) do
      %{
        "area_code" => area_code,
        "exchange_code" => exchange_code,
        "subscriber_number" => subscriber_number
      } ->
        area_code <> exchange_code <> subscriber_number

      nil ->
        "0000000000"
    end
  end

  @doc """
  Extract the area code from a phone number

  Returns the first three digits from a phone number,
  ignoring long distance indicator

  ## Examples

  iex> Phone.area_code("212-555-0100")
  "212"

  iex> Phone.area_code("+1 (212) 555-0100")
  "212"

  iex> Phone.area_code("+1 (012) 555-0100")
  "000"

  iex> Phone.area_code("867.5309")
  "000"
  """
  @spec area_code(String.t()) :: String.t()
  def area_code(raw) do
    case phone_number_parts(raw) do
      %{"area_code" => area_code} ->
        area_code

      nil ->
        "000"
    end
  end

  @doc """
  Pretty print a phone number

  Wraps the area code in parentheses and separates
  exchange and subscriber number with a dash.

  ## Examples

  iex> Phone.pretty("212-555-0100")
  "(212) 555-0100"

  iex> Phone.pretty("212-155-0100")
  "(000) 000-0000"

  iex> Phone.pretty("+1 (303) 555-1212")
  "(303) 555-1212"

  iex> Phone.pretty("867.5309")
  "(000) 000-0000"
  """
  @spec pretty(String.t()) :: String.t()
  def pretty(raw) do
    case phone_number_parts(raw) do
      %{
        "area_code" => area_code,
        "exchange_code" => exchange_code,
        "subscriber_number" => subscriber_number
      } ->
        "(#{area_code}) #{exchange_code}-#{subscriber_number}"

      nil ->
        "(000) 000-0000"
    end
  end

  defp phone_number_parts(raw) do
    Regex.named_captures(@valid_number, raw)
  end
end
