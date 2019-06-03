module BookStore
  BASE_PRICE = 8.0
  private_constant :BASE_PRICE
  BUNDLE_TYPES = (1..5).freeze
  private_constant :BUNDLE_TYPES
  DISCOUNT_TYPES = [0.0, 0.05, 0.1, 0.2, 0.25].freeze
  private_constant :DISCOUNT_TYPES
  BUNDLE_PRICE = lambda do |(bundle, discount)|
    [bundle, BASE_PRICE * bundle * (1 - discount)]
  end
  BUNDLE_PRICES =
    BUNDLE_TYPES
    .zip(DISCOUNT_TYPES)
    .map(&BUNDLE_PRICE)
    .to_h
  private_constant :BUNDLE_PRICES
  NO_COST = 0
  private_constant :NO_COST

  module_function

  def calculate_price(basket)
    return NO_COST if basket.empty?

    basket
      .then(&method(:generate_initial_book_tally))
      .then(&method(:price_books))
  end

  def price_books(book_tally)
    book_tally = remove_zero_tallies(book_tally)
    return 0 if book_tally.empty?

    (1..book_tally.length)
      .each
      .with_object(book_tally)
      .map(&method(:price_bundle))
      .min
  end
  private_class_method :price_books

  def generate_initial_book_tally(basket)
    basket
      .group_by(&:itself)
      .values
      .map(&:length)
  end
  private_class_method :generate_initial_book_tally

  def remove_zero_tallies(book_tally)
    book_tally
      .sort
      .drop_while(&:zero?)
      .reverse
  end
  private_class_method :remove_zero_tallies

  def price_bundle(bundle, book_tally)
    head = book_tally.take(bundle).map(&:pred)
    tail = book_tally.drop(bundle)
    next_bundle_tally = head.push(*tail)

    price_books(next_bundle_tally) + BUNDLE_PRICES[bundle]
  end
  private_class_method :price_bundle
end
