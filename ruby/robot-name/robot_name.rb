# frozen_string_literal: true

require "forwardable"

class Robot
  module Names
    POSSIBLE_NAMES = ("AA000".."ZZ999").to_a.freeze
    private_constant :POSSIBLE_NAMES
    TOTAL_POSSIBLE_NAME_COUNT = POSSIBLE_NAMES.size
    private_constant :TOTAL_POSSIBLE_NAME_COUNT
    OutOfNamesError = Class.new(StandardError)
    private_constant :OutOfNamesError

    module_function

    def assign
      raise OutOfNamesError unless name_assignable?
      @assigned_names += 1
      @names.shift
    end

    def forget(rng = Random.new)
      @assigned_names = 0
      @names = POSSIBLE_NAMES.shuffle(random: rng)
    end

    def name_assignable?
      @assigned_names < TOTAL_POSSIBLE_NAME_COUNT
    end
    private_class_method :name_assignable?

    forget
  end

  extend SingleForwardable

  attr_reader :name

  def_delegator self::Names, :forget

  def initialize
    reset
  end

  def reset
    self.name = Names.assign
  end

  private

  attr_writer :name
end

module BookKeeping
  VERSION = 3
end
