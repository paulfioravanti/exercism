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

  def encode(plaintext)
    shift(plaintext, :+)
  end

  def decode(ciphertext)
    shift(ciphertext, :-)
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

  def substitute((text_char, key_char), message)
    [text_char, key_char]
      .map { |char| char.ord - A_ORDINAL }
      .reduce(message)
      .modulo(ALPHABET.count)
      .+(A_ORDINAL)
      .chr
  end
end
