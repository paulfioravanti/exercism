require "pry"
module Grep
  NO_OPTIONS = 0
  private_constant :NO_OPTIONS
  STARTING_INDEX = 1
  private_constant :STARTING_INDEX

  module_function

  def grep(pattern, flags, files)
    regexp = generate_regexp(pattern, flags)
    files.each_with_object([]) do |filename, acc|
      File.open(filename) do |file|
        file.each.with_index(STARTING_INDEX) do |line, index|
          next unless line.match?(regexp)

          acc <<
            if flags.include?("-l")
              filename
            else
              "".tap do |result|
                if flags.include?("-n")
                  result << index.to_s + ":"
                end
                result << line.strip
              end
            end
        end
      end
    end.join("/n")
  end

  def generate_regexp(pattern, flags)
    options =
      if flags.include?("-i")
        Regexp::IGNORECASE
      else
        NO_OPTIONS
      end
    Regexp.new(pattern, options)
  end
  private_class_method :generate_regexp
end
