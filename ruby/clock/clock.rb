# frozen_string_literal: true

class Clock
  TIME_FORMAT = "%02d:%02d"
  private_constant :TIME_FORMAT
  CLOCK_MINUTES = (0..59).freeze
  private_constant :CLOCK_MINUTES
  CLOCK_HOURS = (0..23).freeze
  private_constant :CLOCK_HOURS
  MINUTES_IN_HOUR = 60
  private_constant :MINUTES_IN_HOUR
  HOURS_IN_DAY = 24
  private_constant :HOURS_IN_DAY

  def initialize(hour: 0, minute: 0)
    @hour = hour
    @minute = minute
  end

  def +(other)
    tap { |clock| clock.minute = minute + other.minute }
  end

  def -(other)
    tap { |clock| clock.minute = minute - other.minute }
  end

  def ==(other)
    to_s == other.to_s
  end

  def to_s
    display_minutes = minute
    display_hour = hour

    unless CLOCK_MINUTES.cover?(display_minutes)
      hours_to_add, display_minutes = display_minutes.divmod(MINUTES_IN_HOUR)
      display_hour += hours_to_add
    end

    display_hour %= HOURS_IN_DAY unless CLOCK_HOURS.cover?(display_hour)

    format(TIME_FORMAT, display_hour, display_minutes)
  end

  protected

  attr_accessor :minute

  private

  attr_reader :hour
end
