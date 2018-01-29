# frozen_string_literal: true

class Cipher
  A_ORDINAL = "a".ord
  private_constant :A_ORDINAL
  ALPHABET = ("a".."z").to_a.freeze
  private_constant :ALPHABET
  RANDOM_KEY = -> { (0...100).map { ALPHABET.sample }.join }
  private_constant :RANDOM_KEY
  VALID_KEY = /\A[a-z]+\z/
  private_constant :VALID_KEY

  attr_reader :key

  def initialize(key = RANDOM_KEY.call)
    raise ArgumentError unless key.match?(VALID_KEY)
    @key = key
  end

  def encode(text)
    shift(text, :+)
  end

  def decode(text)
    shift(text, :-)
  end

  private

  def shift(text, message)
    text
      .chars
      .lazy
      .zip(key.chars.cycle)
      .with_object(message)
      .map(&method(:substitute))
      .force
      .join
  end

  def substitute((char, key_char), message)
    [char, key_char]
      .map { |num| num.ord - A_ORDINAL }
      .reduce(message)
      .modulo(ALPHABET.count)
      .+(A_ORDINAL)
      .chr
  end
end
