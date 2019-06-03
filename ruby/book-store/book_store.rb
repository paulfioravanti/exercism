require "pry"
module BookStore
  BASE_BOOK_PRICE = 8.0
  private_constant :BASE_BOOK_PRICE
  DISCOUNT_TYPES = {
    1 => 0.0,
    2 => 0.05,
    3 => 0.1,
    4 => 0.2,
    5 => 0.25
  }.freeze
  private_constant :DISCOUNT_TYPES
  NO_COST = 0.0
  private_constant :NO_COST

  module_function

  def calculate_price(basket)
    return NO_COST if basket.empty?

    grouped_basket = group_basket(basket)
    num_book_types = grouped_basket.length
    percent_discount = DISCOUNT_TYPES[num_book_types]

    grouped_basket.reduce(0) do |acc, books|
      price = books.length * BASE_BOOK_PRICE
      discounted_price = price * (1 - percent_discount)
      acc + discounted_price
    end
  end

  def group_basket(basket)
    basket
      .group_by(&:itself)
      .values
      .sort_by(&:length)
      .reverse
  end

  def discounted_price(volume)
    BASE_BOOK_PRICE * volume * (1 - DISCOUNT_TYPES[volume])
  end
end
