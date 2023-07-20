# frozen_string_literal: true

class Robot
  module Names
    POSSIBLE_NAMES = ("AA000".."ZZ999").to_a.freeze
    private_constant :POSSIBLE_NAMES
    COUNT = POSSIBLE_NAMES.size
    private_constant :COUNT

    OutOfNamesError = Class.new(StandardError)

    module_function

    def generate
      raise OutOfNamesError if out_of_names?

      @assigned_names += 1
      @names.shift
    end

    def forget
      @assigned_names = 0
      @names = POSSIBLE_NAMES.shuffle(random: Random.new)
    end

    private_class_method def out_of_names?
      @assigned_names >= COUNT
    end

    forget
  end
  private_constant :Names

  attr_reader :name

  def self.forget
    Names.forget
  end

  def initialize
    reset
  end

  def reset
    self.name = Names.generate
  end

  private

  attr_writer :name
end
