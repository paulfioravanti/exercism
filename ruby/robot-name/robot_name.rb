# frozen_string_literal: true

class Robot
  POSSIBLE_NAMES = ("AA000".."ZZ999").to_a.freeze
  private_constant :POSSIBLE_NAMES

  attr_reader :name

  class << self
    attr_reader :names

    def forget
      @names = POSSIBLE_NAMES.shuffle(random: Random.new)
    end
  end

  def initialize
    reset
  end

  def reset
    self.name = self.class.names.shift
  end

  private

  attr_writer :name
end

module BookKeeping
  VERSION = 3
end
