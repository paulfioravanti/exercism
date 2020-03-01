const OUTSIDE_TARGET_POINTS = 0
const OUTER_CIRCLE_BOUNDARY = 10
const OUTER_CIRCLE_POINTS = 1
const MIDDLE_CIRCLE_BOUNDARY = 5
const MIDDLE_CIRCLE_POINTS = 5
const INNER_CIRCLE_BOUNDARY = 1
const INNER_CIRCLE_POINTS = 10
const CALCULATE_POINT = (x, y) => Math.sqrt(x**2 + y**2)

export const score = (x, y) => {
  const point = CALCULATE_POINT(x, y)

  if (point > OUTER_CIRCLE_BOUNDARY) {
    return OUTSIDE_TARGET_POINTS
  }

  if (point > MIDDLE_CIRCLE_BOUNDARY) {
    return OUTER_CIRCLE_POINTS
  }

  if (point > INNER_CIRCLE_BOUNDARY) {
    return MIDDLE_CIRCLE_POINTS
  }

  return INNER_CIRCLE_POINTS
}
