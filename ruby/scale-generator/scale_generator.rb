# frozen_string_literal: true

class Scale
  FLAT_SCALE_TONICS = %w[F Bb Eb Ab Db Gb d g c f bb eb].freeze
  private_constant :FLAT_SCALE_TONICS
  FLAT_SCALE = %w[C Db D Eb E F Gb G Ab A Bb B].freeze
  private_constant :FLAT_SCALE
  INITIAL_SCALE_INDEX = 0
  private_constant :INITIAL_SCALE_INDEX
  INTERVALS = { "m" => 1, "M" => 2, "A" => 3 }.freeze
  private_constant :INTERVALS
  NAME_FORMAT = "%<tonic>s %<scale_name>s"
  private_constant :NAME_FORMAT
  SHARP_SCALE = %w[C C# D D# E F F# G G# A A# B].freeze
  private_constant :SHARP_SCALE

  def initialize(tonic, scale_name, intervals = nil)
    @tonic = tonic.capitalize
    @scale_name = scale_name
    @intervals = intervals
    @scale = FLAT_SCALE_TONICS.include?(tonic) ? FLAT_SCALE : SHARP_SCALE
  end

  def name
    format(NAME_FORMAT, tonic: tonic, scale_name: scale_name)
  end

  def pitches
    tonic_scale = scale_for_tonic
    return tonic_scale unless intervals

    last_index = 0
    intervals.chars.each_with_object([]) do |interval, acc|
      acc << tonic_scale[last_index]
      last_index += INTERVALS[interval]
    end
  end

  private

  attr_reader :tonic, :scale_name, :intervals, :scale

  def scale_for_tonic
    scale
      .index(tonic)
      .then { |index| scale.rotate(index) }
  end
end
