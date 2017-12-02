# frozen_string_literal: true

class Crypto
  SPACES_AND_PUNCTUATION = /[[[:space:]][[:punct:]]]/
  private_constant :SPACES_AND_PUNCTUATION

  def initialize(plaintext)
    @plaintext = plaintext
  end

  def ciphertext
    return plaintext if plaintext.empty?
    encode(rectangle)
  end

  private

  attr_reader :plaintext

  def rectangle
    collection = normalised_plaintext.chars
    padding = (columns - collection.length % columns) % columns
    collection.concat(Array.new(padding, " ")).each_slice(columns).to_a
  end

  def normalised_plaintext
    @normalised_plaintext ||=
      plaintext.downcase.strip.gsub(SPACES_AND_PUNCTUATION, "")
  end

  def columns
    @columns ||= Math.sqrt(normalised_plaintext.length).ceil
  end

  def encode(rectangle)
    rectangle.transpose.map(&:join).join(" ")
  end
end

module BookKeeping
  VERSION = 1
end
