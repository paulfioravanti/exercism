defmodule RobotSimulator do
  defstruct direction: :north, position: {0, 0}

  @bearings [:north, :east, :south, :west]
  @instructions %{
    "A" => :advance,
    "L" => :turn_left,
    "R" => :turn_right
  }

  alias __MODULE__, as: RobotSimulator

  defguardp invalid_direction?(direction) when not (direction in @bearings)

  defguardp invalid_position?(position)
            when not is_tuple(position) or
                   tuple_size(position) != 2 or
                   not is_integer(elem(position, 0)) or
                   not is_integer(elem(position, 1))

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: atom, position :: {integer, integer}) :: any
  def create(direction \\ nil, position \\ nil)
  def create(nil, nil), do: %RobotSimulator{}

  def create(direction, _position) when invalid_direction?(direction) do
    {:error, "invalid direction"}
  end

  def create(_direction, position) when invalid_position?(position) do
    {:error, "invalid position"}
  end

  def create(direction, position) do
    %RobotSimulator{direction: direction, position: position}
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: any, instructions :: String.t()) :: any
  def simulate(robot, instructions) do
    instructions
    |> String.graphemes()
    |> Enum.map(&parse_instruction/1)
    |> Enum.reduce(robot, &apply(RobotSimulator, &1, [&2]))
  catch
    :invalid_instruction ->
      {:error, "invalid instruction"}
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: any) :: atom
  def direction(%RobotSimulator{direction: direction}), do: direction

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: any) :: {integer, integer}
  def position(%RobotSimulator{position: position}), do: position

  defp parse_instruction(instruction) do
    case Map.fetch(@instructions, instruction) do
      {:ok, instruction} ->
        instruction

      :error ->
        throw(:invalid_instruction)
    end
  end

  def turn_left(robot) do
    @bearings
    |> right_rotate(1)
    |> turn(robot)
  end

  def turn_right(robot) do
    @bearings
    |> left_rotate(1)
    |> turn(robot)
  end

  def advance(robot) do
    {x, y} = robot.position

    new_position =
      case robot.direction do
        :north ->
          {x, y + 1}

        :east ->
          {x + 1, y}

        :south ->
          {x, y - 1}

        :west ->
          {x - 1, y}
      end

    %RobotSimulator{robot | position: new_position}
  end

  defp turn(new_bearings, robot) do
    index =
      @bearings
      |> Enum.find_index(&(&1 == robot.direction))

    new_direction =
      new_bearings
      |> Enum.fetch!(index)

    %RobotSimulator{robot | direction: new_direction}
  end

  defp left_rotate(list, 0), do: list

  defp left_rotate([head | tail], count) do
    left_rotate(tail ++ [head], count - 1)
  end

  defp right_rotate(list, count) do
    list
    |> Enum.reverse()
    |> left_rotate(count)
    |> Enum.reverse()
  end
end
