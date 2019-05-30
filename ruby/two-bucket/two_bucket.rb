# frozen_string_literal: true

class TwoBucket
  class Bucket
    EMPTY = 0
    private_constant :EMPTY

    attr_reader :name, :capacity, :contents

    def initialize(name, capacity)
      @name = name
      @capacity = capacity
      @contents = EMPTY
    end

    def empty
      self.contents = EMPTY
    end

    def empty?
      contents == EMPTY
    end

    def fill
      self.contents = capacity
      self
    end

    def full?
      contents == capacity
    end

    def pour_fluid_into(other)
      amount = calculate_fluid_amount(other)
      other.contents += amount
      self.contents -= amount
    end

    protected

    attr_writer :contents

    private

    def calculate_fluid_amount(other)
      [other.capacity - other.contents, contents].min
    end
  end
  private_constant :Bucket

  ONE = "one"
  private_constant :ONE
  TWO = "two"
  private_constant :TWO

  attr_reader :goal_bucket, :moves, :other_bucket

  def initialize(capacity1, capacity2, goal, start_bucket)
    @goal = goal
    @goal_bucket, @other_bucket =
      [capacity1, capacity2, start_bucket]
      .then(&method(:initialize_buckets))
      .then(&method(:initialize_moves))
  end

  private

  attr_reader :goal
  attr_writer :moves

  def initialize_buckets((capacity1, capacity2, start_bucket))
    bucket_one = Bucket.new(ONE, capacity1)
    bucket_two = Bucket.new(TWO, capacity2)
    self.moves = 1
    if start_bucket == ONE
      [bucket_one.fill, bucket_two]
    else
      [bucket_two.fill, bucket_one]
    end
  end

  def initialize_moves((start_bucket, other_bucket))
    loop do
      return [start_bucket.name, other_bucket.contents] if
        start_bucket.contents == goal
      return [other_bucket.name, start_bucket.contents] if
        other_bucket.contents == goal

      transfer_fluid(start_bucket, other_bucket)
      self.moves += 1
    end
  end

  def transfer_fluid(start_bucket, other_bucket)
    if start_bucket.empty?
      start_bucket.fill
    elsif other_bucket.capacity == goal
      other_bucket.fill
    elsif other_bucket.full?
      other_bucket.empty
    else
      start_bucket.pour_fluid_into(other_bucket)
    end
  end
end
