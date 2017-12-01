module Gigasecond
  SECONDS = 10**9
  private_constant :SECONDS

  module_function

  def from(date_of_birth)
    date_of_birth + SECONDS
  end
end

module BookKeeping
  VERSION = 6
end
