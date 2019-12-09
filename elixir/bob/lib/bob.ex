defmodule Bob do
  def hey(input) do
    remark = String.trim(input)

    if silence?(remark) do
      "Fine. Be that way!"
    else
      respond_to_verbal_remark(remark)
    end
  end

  defp respond_to_verbal_remark(remark) do
    case {question?(remark), yelling?(remark)} do
      {true, true} ->
        "Calm down, I know what I'm doing!"

      {true, false} ->
        "Sure."

      {false, true} ->
        "Whoa, chill out!"

      {false, false} ->
        "Whatever."
    end
  end

  defp silence?(remark), do: remark == ""
  defp question?(remark), do: String.ends_with?(remark, "?")

  defp yelling?(remark) do
    remark == String.upcase(remark) and remark != String.downcase(remark)
  end
end
