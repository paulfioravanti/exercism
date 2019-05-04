module RailFenceCipher
  RAIL_RANGE = ->(offset, limit) { (offset...(limit - offset)).to_a }
  private_constant :RAIL_RANGE
  RAILS = ->(n) { Array.new(n) { [] } }
  private_constant :RAILS
  ZIG_ZAG_PATTERN = lambda { |limit|
    (RAIL_RANGE.call(0, limit) + RAIL_RANGE.call(1, limit).reverse).cycle
  }
  private_constant :ZIG_ZAG_PATTERN

  module_function

  def encode(message, num_rails)
    pattern_iterator = ZIG_ZAG_PATTERN.call(num_rails)
    zig_zag_transpose(message, num_rails, pattern_iterator).join
  end

  def decode(message, num_rails)
    pattern = ZIG_ZAG_PATTERN.call(num_rails).first(message.length)
    message_on_rails =
      zig_zag_transpose(message, num_rails, pattern.sort.each)
    pattern.map { |index| message_on_rails[index].shift }.join
  end

  def zig_zag_transpose(message, num_rails, pattern_iterator)
    message.chars.each_with_object(RAILS.call(num_rails)) do |char, rails|
      rails[pattern_iterator.next] << char
    end
  end
  private_class_method :zig_zag_transpose
end
