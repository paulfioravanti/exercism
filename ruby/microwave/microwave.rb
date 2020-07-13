# frozen_string_literal: true

class Microwave
  module Granularity
    SECONDS = 60
    MINUTES = 100
  end
  private_constant :Granularity
  MAX_SECONDS = 99
  private_constant :MAX_SECONDS
  TIME_DISPLAY = "%<minutes>02d:%<seconds>02d"
  private_constant :TIME_DISPLAY

  def initialize(timer)
    granularity = determine_granularity(timer)
    @minutes, @seconds = timer.divmod(granularity)
  end

  def timer
    format(TIME_DISPLAY, minutes: minutes, seconds: seconds)
  end

  private

  attr_reader :minutes, :seconds

  def determine_granularity(timer)
    timer > MAX_SECONDS ? Granularity::MINUTES : Granularity::SECONDS
  end
end
