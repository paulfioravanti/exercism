class Darts
  OUTSIDE_TARGET_POINTS = 0
  private_constant :OUTSIDE_TARGET_POINTS
  OUTER_CIRCLE_BOUNDARY = 10
  private_constant :OUTER_CIRCLE_BOUNDARY
  OUTER_CIRCLE_POINTS = 1
  private_constant :OUTER_CIRCLE_POINTS
  MIDDLE_CIRCLE_BOUNDARY = 5
  private_constant :MIDDLE_CIRCLE_BOUNDARY
  MIDDLE_CIRCLE_POINTS = 5
  private_constant :MIDDLE_CIRCLE_POINTS
  INNER_CIRCLE_BOUNDARY = 1
  private_constant :INNER_CIRCLE_BOUNDARY
  INNER_CIRCLE_POINTS = 10
  private_constant :INNER_CIRCLE_POINTS
  CALCULATE_POINT = ->(x_coord, y_coord) { Math.sqrt(x_coord**2 + y_coord**2) }
  private_constant :CALCULATE_POINT

  attr_reader :point

  def initialize(x_coord, y_coord)
    @point = CALCULATE_POINT.call(x_coord, y_coord)
  end

  def score
    if point > OUTER_CIRCLE_BOUNDARY
      OUTSIDE_TARGET_POINTS
    elsif point > MIDDLE_CIRCLE_BOUNDARY
      OUTER_CIRCLE_POINTS
    elsif point > INNER_CIRCLE_BOUNDARY
      MIDDLE_CIRCLE_POINTS
    else
      INNER_CIRCLE_POINTS
    end
  end
end
