class Triplet
  NUM_SIDES = 3
  private_constant :NUM_SIDES

  def self.where(min_factor: 1, max_factor:, sum: nil)
    (min_factor..max_factor)
      .to_a
      .combination(NUM_SIDES)
      .with_object(sum)
      .each_with_object([], &method(:add_pythagorean_triplet))
  end

  def self.add_pythagorean_triplet(((a, b, c), sum), acc)
    triplet = Triplet.new(a, b, c)
    acc << triplet if pythagorean_triplet?(triplet, sum)
  end
  private_class_method :add_pythagorean_triplet

  def self.pythagorean_triplet?(triplet, sum)
    triplet.pythagorean? && (triplet.sum == sum || sum.nil?)
  end
  private_class_method :pythagorean_triplet?

  def initialize(a_num, b_num, c_num)
    @sides = [a_num, b_num, c_num].sort
    @a, @b, @c = sides
  end

  def sum
    sides.sum
  end

  def product
    sides.reduce(&:*)
  end

  def pythagorean?
    a**2 + b**2 == c**2
  end

  private

  attr_reader :a, :b, :c, :sides
end
