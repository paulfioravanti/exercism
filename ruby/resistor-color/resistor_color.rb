# frozen_string_literal: true

module ResistorColor
  COLORS = %w[
    black
    brown
    red
    orange
    yellow
    green
    blue
    violet
    grey
    white
  ].freeze

  module_function

  def color_code(color)
    COLORS.index(color)
  end
end
