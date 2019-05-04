class Palindromes
  def initialize(min_factor: 0, max_factor:)
    @range = (min_factor..max_factor).to_a
  end

  def generate
    @products = generate_palindrome_products
  end

  def largest
    @largest ||= palindrome(:max)
  end

  def smallest
    @smallest ||= palindrome(:min)
  end

  private

  attr_reader :range, :products

  def generate_palindrome_products
    range
      .product(range)
      .map { |x, y| x * y }
      .uniq
      .select { |product| palindrome?(product) }
  end

  def palindrome?(number)
    number == number.to_s.reverse.to_i
  end

  def palindrome(method)
    Palindrome.new(range, products.public_send(method))
  end

  class Palindrome
    attr_reader :factors, :value

    def initialize(range, value)
      @value = value
      @factors = generate_factors(range)
    end

    private

    def generate_factors(range)
      range
        .repeated_combination(2)
        .to_a
        .select { |x, y| x * y == value }
    end
  end
  private_constant :Palindrome
end
