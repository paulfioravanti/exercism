# frozen_string_literal: true

module Grep
  COLON = ":"
  private_constant :COLON
  ENTIRE_LINE = ->(pattern) { "\\A#{pattern}\n\\z" }
  private_constant :ENTIRE_LINE
  ENTIRE_LINES_ONLY = "-x"
  private_constant :ENTIRE_LINES_ONLY
  FILENAMES_ONLY = "-l"
  private_constant :FILENAMES_ONLY
  IGNORE_CASE = "-i"
  private_constant :IGNORE_CASE
  INVERT_PATTERN = "-v"
  private_constant :INVERT_PATTERN
  LINE_NUMBERS = "-n"
  private_constant :LINE_NUMBERS
  NEWLINE = "\n"
  private_constant :NEWLINE
  NO_OPTIONS = 0
  private_constant :NO_OPTIONS
  STARTING_INDEX = 1
  private_constant :STARTING_INDEX

  module_function

  def grep(pattern, flags, files)
    regexp = generate_regexp(pattern, flags)
    multiple_files = files.length > 1
    line_guard = generate_line_guard(flags)

    files
      .each
      .with_object([flags, regexp, multiple_files, line_guard])
      .each_with_object([], &method(:grep_file))
      .join(NEWLINE)
  end

  def generate_regexp(pattern, flags)
    string =
      flags.include?(ENTIRE_LINES_ONLY) ? ENTIRE_LINE.call(pattern) : pattern
    options =
      flags.include?(IGNORE_CASE) ? Regexp::IGNORECASE : NO_OPTIONS
    Regexp.new(string, options)
  end
  private_class_method :generate_regexp

  def generate_line_guard(flags)
    if flags.include?(INVERT_PATTERN)
      ->(line, regexp) { throw(:next) if line.match?(regexp) }
    else
      ->(line, regexp) { throw(:next) unless line.match?(regexp) }
    end
  end
  private_class_method :generate_line_guard

  def grep_file((filename, (flags, regexp, multiple_files, line_guard)), acc)
    File.open(filename) do |file|
      object = [filename, flags, regexp, multiple_files, line_guard, acc]
      file
        .each
        .with_index(STARTING_INDEX)
        .with_object(object, &method(:grep_line))
    end
  end
  private_class_method :grep_file

  def grep_line((line, index), object)
    filename, flags, regexp, multiple_files, line_guard, acc = object
    catch(:next) do
      line_guard.call(line, regexp)
      add_filename_only(flags, filename, acc)
      acc << add_line(flags, multiple_files, filename, index, line)
    end
  end
  private_class_method :grep_line

  def add_filename_only(flags, filename, acc)
    return unless flags.include?(FILENAMES_ONLY)

    throw(:next) if acc.include?(filename)
    acc << filename
    throw(:next)
  end
  private_class_method :add_filename_only

  def add_line(flags, multiple_files, filename, index, line)
    String.new.tap do |result|
      result << filename + COLON if multiple_files
      result << index.to_s + COLON if flags.include?(LINE_NUMBERS)
      result << line.strip
    end
  end
  private_class_method :add_line
end
