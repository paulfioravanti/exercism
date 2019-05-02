defmodule Bob do
  def hey(input) do
    remark = String.trim(input)

    cond do
      silence?(remark) ->
        "Fine. Be that way!"

      yelling_question?(remark) ->
        "Calm down, I know what I'm doing!"

      asking_question?(remark) ->
        "Sure."

      yelling?(remark) ->
        "Whoa, chill out!"

      true ->
        "Whatever."
    end
  end

  defp silence?(remark) do
    remark == ""
  end

  defp yelling_question?(remark) do
    asking_question?(remark) and yelling?(remark)
  end

  defp asking_question?(remark) do
    String.ends_with?(remark, "?")
  end

  defp yelling?(remark) do
    remark == String.upcase(remark) and remark != String.downcase(remark)
  end
end
