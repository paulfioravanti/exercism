module Grep
  module_function

  def grep(pattern, _flags, files)
    files.each_with_object([]) do |file, acc|
      IO.foreach(file) do |line|
        acc << line.strip if line.include?(pattern)
      end
    end.join("/n")
  end
end
