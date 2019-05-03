require "english"

module RunLengthEncoding
  CONSECUTIVE_DATA_ELEMENTS = /([A-Za-z\s])\1+/.freeze
  private_constant :CONSECUTIVE_DATA_ELEMENTS
  RUN_LENGTH_ENCODING = /(?<count>\d+)(?<character>\D)/.freeze
  private_constant :RUN_LENGTH_ENCODING

  module_function

  def encode(input)
    return "" if input.empty?

    input.gsub(CONSECUTIVE_DATA_ELEMENTS) do
      "#{$MATCH.length}#{$LAST_PAREN_MATCH}"
    end
  end

  def decode(input)
    return "" if input.empty?

    input.gsub(RUN_LENGTH_ENCODING) do
      $LAST_MATCH_INFO[:character] * $LAST_MATCH_INFO[:count].to_i
    end
  end
end
