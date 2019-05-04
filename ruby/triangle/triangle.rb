class Triangle
  IS_EQUILATERAL = ->(unique_sides) { unique_sides == 1 }
  private_constant :IS_EQUILATERAL
  IS_ISOCELES = ->(unique_sides) { unique_sides < 3 }
  private_constant :IS_ISOCELES
  IS_SCALENE = ->(unique_sides) { unique_sides == 3 }
  private_constant :IS_SCALENE

  def initialize(sides)
    @sides = sides.sort
  end

  def equilateral?
    legal_triangle?(&IS_EQUILATERAL)
  end

  def isosceles?
    legal_triangle?(&IS_ISOCELES)
  end

  def scalene?
    legal_triangle?(&IS_SCALENE)
  end

  private

  attr_reader :sides

  def legal_triangle?(&condition)
    sides.all?(&:positive?) &&
      side_lengths_all_legal? &&
      sides.uniq.size.yield_self(&condition)
  end

  def side_lengths_all_legal?
    sides.yield_self do |(*first_two_sides, third_side)|
      first_two_sides.sum >= third_side
    end
  end
end
