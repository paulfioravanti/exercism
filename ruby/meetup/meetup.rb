# frozen_string_literal: true

require "date"

class Meetup
  DATE_BY_DESCRIPTOR = lambda do |dates, descriptor|
    case descriptor
    when :first
      dates[0]
    when :second
      dates[1]
    when :third
      dates[2]
    when :fourth
      dates[3]
    when :last
      dates.last
    when :teenth
      dates.find { |date| date.mday.between?(13, 19) }
    end
  end
  private_constant :DATE_BY_DESCRIPTOR

  def initialize(month, year)
    @month = Date.new(year, month)..Date.new(year, month, -1)
  end

  def day(day, descriptor)
    DATE_BY_DESCRIPTOR.call(month.select(&:"#{day}?"), descriptor)
  end

  private

  attr_reader :month
end
