# frozen_string_literal: true

class SecretHandshake
  ACTIONS = [
    [:<<, "wink"],
    [:<<, "double blink"],
    [:<<, "close your eyes"],
    [:<<, "jump"],
    [:reverse!]
  ].freeze
  private_constant :ACTIONS
  NUMBER = /\A\d+\z/.freeze
  private_constant :NUMBER
  MASK = ->(number) { 1 << number }
  private_constant :MASK

  def initialize(number)
    @number = number
  end

  def commands
    return [] unless number.to_s.match?(NUMBER)

    ACTIONS.each.with_index.with_object([]) do |(action, index), acc|
      acc.public_send(*action) if set?(index)
    end
  end

  private

  def set?(index)
    (number & MASK[index]).positive?
  end

  attr_reader :number
end
