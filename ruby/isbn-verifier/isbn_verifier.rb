# frozen_string_literal: true

module IsbnVerifier
  CHECK = -1
  private_constant :CHECK
  CONVERT_CHECK = ->(check) { check == "X" ? 10 : check.to_i }
  private_constant :CONVERT_CHECK
  DASHES = "-"
  private_constant :DASHES
  INITIAL_SUM = 0
  private_constant :INITIAL_SUM
  ISBN_10 = /\A\d{9}(\d|X)\z/.freeze
  private_constant :ISBN_10
  MULTIPLE = 11
  private_constant :MULTIPLE
  WEIGHTS = (1..10).to_a.reverse.freeze
  private_constant :WEIGHTS
  WITHOUT_CHECK = (0..-2).freeze
  private_constant :WITHOUT_CHECK

  module_function

  def valid?(string)
    string = string.delete(DASHES)
    return false unless string.match?(ISBN_10)

    string
      .then(&method(:generate_isbn_integers))
      .zip(WEIGHTS)
      .reduce(INITIAL_SUM, &method(:isbn_formula))
      .modulo(MULTIPLE)
      .zero?
  end

  def generate_isbn_integers(string)
    check = CONVERT_CHECK.call(string[CHECK])

    string
      .chars[WITHOUT_CHECK]
      .map(&:to_i)
      .append(check)
  end
  private_class_method :generate_isbn_integers

  def isbn_formula(acc, (integer, weight))
    acc + integer * weight
  end
  private_class_method :isbn_formula
end
