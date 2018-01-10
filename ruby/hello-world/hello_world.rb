# frozen_string_literal: true

module HelloWorld
  HELLO = "Hello, %<name>s!"
  private_constant :HELLO
  DEFAULT = "World"
  private_constant :DEFAULT

  module_function

  def hello(name = DEFAULT)
    format(HELLO, name: name)
  end
end
