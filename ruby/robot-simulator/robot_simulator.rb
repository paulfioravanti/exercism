# frozen_string_literal: true

class Robot
  BEARINGS = %i[north east south west].freeze
  private_constant :BEARINGS
  LEFT = -1
  private_constant :LEFT
  RIGHT = 1
  private_constant :RIGHT

  attr_reader :bearing, :coordinates

  def orient(direction)
    raise ArgumentError unless BEARINGS.include?(direction)

    self.bearing = direction
  end

  def at(x_coord, y_coord)
    self.coordinates = [x_coord, y_coord]
  end

  def advance
    x, y = coordinates
    case bearing
    when :north then at(x, y + 1)
    when :east then at(x + 1, y)
    when :south then at(x, y - 1)
    else at(x - 1, y)
    end
  end

  def turn_left
    turn(LEFT)
  end

  def turn_right
    turn(RIGHT)
  end

  private

  attr_writer :bearing, :coordinates

  def turn(direction)
    bearing
      .then(&BEARINGS.method(:index))
      .then { |index| BEARINGS.rotate(direction)[index] }
      .then(&method(:orient))
  end
end

class Simulator
  INSTRUCTIONS = {
    "A" => :advance,
    "L" => :turn_left,
    "R" => :turn_right
  }.freeze
  private_constant :INSTRUCTIONS

  def instructions(input)
    input.chars.map(&INSTRUCTIONS)
  end

  # rubocop:disable Naming/UncommunicativeMethodParamName
  def place(robot, x:, y:, direction:)
    robot.at(x, y)
    robot.orient(direction)
  end
  # rubocop:enable Naming/UncommunicativeMethodParamName

  def evaluate(robot, input)
    input
      .then(&method(:instructions))
      .each
      .with_object(robot, &method(:instruct_robot))
  end

  private

  def instruct_robot(instruction, robot)
    robot.public_send(instruction)
  end
end
