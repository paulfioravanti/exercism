# frozen_string_literal: true

class Clock
  TIME_FORMAT = "%02d:%02d"
  private_constant :TIME_FORMAT
  CLOCK_MINUTES = (0..59)
  private_constant :CLOCK_MINUTES
  CLOCK_HOURS = (0..23)
  private_constant :CLOCK_HOURS
  MINUTES_IN_HOUR = 60
  private_constant :MINUTES_IN_HOUR
  HOURS_IN_DAY = 24
  private_constant :HOURS_IN_DAY

  def self.at(hours, minutes)
    new(hours, minutes)
  end

  private_class_method :new

  def initialize(hour, minutes)
    @hour = hour
    @minutes = minutes
  end

  def +(other)
    tap { |clock| clock.send(:minutes=, minutes + other) }
  end

  def ==(other)
    to_s == other.to_s
  end

  def to_s
    display_minutes = minutes
    display_hour = hour

    unless CLOCK_MINUTES.cover?(display_minutes)
      hours_to_add, display_minutes = display_minutes.divmod(MINUTES_IN_HOUR)
      display_hour += hours_to_add
    end

    display_hour %= HOURS_IN_DAY unless CLOCK_HOURS.cover?(display_hour)

    format(TIME_FORMAT, display_hour, display_minutes)
  end

  private

  attr_reader :hour
  attr_accessor :minutes
end

module BookKeeping
  VERSION = 2
end
