# frozen_string_literal: true

class Affine
  ALPHABET = "abcdefghijklmnopqrstuvwxyz"
  private_constant :ALPHABET
  CHUNKS_OF_5 = /.{1,5}/.freeze
  private_constant :CHUNKS_OF_5
  IS_COPRIME = ->(a_key) { a_key.gcd(ALPHABET.length) == 1 }
  private_constant :IS_COPRIME
  NON_WORD_CHARACTERS = /\W/.freeze
  private_constant :NON_WORD_CHARACTERS

  def initialize(a_key, b_key)
    raise ArgumentError unless IS_COPRIME.call(a_key)

    @key = generate_key(a_key, b_key)
  end

  def encode(plaintext)
    plaintext
      .downcase
      .gsub(NON_WORD_CHARACTERS, "")
      .tr(ALPHABET, key)
      .scan(CHUNKS_OF_5)
      .join(" ")
  end

  def decode(ciphertext)
    ciphertext
      .gsub(NON_WORD_CHARACTERS, "")
      .tr(key, ALPHABET)
  end

  private

  attr_reader :key

  def generate_key(a_key, b_key)
    ALPHABET
      .chars
      .each_index
      .with_object(a_key)
      .with_object(b_key)
      .reduce("", &method(:affine_character))
  end

  def affine_character(acc, ((index, a_key), b_key))
    # (ax + b) mod m
    affine_index = (a_key * index + b_key) % ALPHABET.length
    acc + ALPHABET[affine_index]
  end
end
