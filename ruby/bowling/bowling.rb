class Game
  class BowlingError < StandardError; end

  FRAMES = 10
  private_constant :FRAMES
  START_FRAME = 0
  private_constant :START_FRAME
  STANDARD_FRAME_SIZE = 2
  private_constant :STANDARD_FRAME_SIZE
  FINAL_FRAME = FRAMES - 1
  private_constant :FINAL_FRAME
  FINAL_FRAME_SIZE = 3
  private_constant :FINAL_FRAME_SIZE
  FINAL_STANDARD_FRAME = FINAL_FRAME - 1
  private_constant :FINAL_STANDARD_FRAME
  FRAME_INCREMENT = 1
  private_constant :FRAME_INCREMENT
  MIN_PINS = 0
  private_constant :MIN_PINS
  MAX_PINS = 10
  private_constant :MAX_PINS
  STANDARD_FRAME_STRIKE = [MAX_PINS].freeze
  private_constant :STANDARD_FRAME_STRIKE
  GAME_OVER = 10
  private_constant :GAME_OVER
  SCORE_SEED = 0
  private_constant :SCORE_SEED

  def initialize
    @frames = Array.new(FRAMES) { [] }
    @frame_number = START_FRAME
    @remaining_pins = MAX_PINS
  end

  def roll(pins)
    raise BowlingError if invalid_roll?(pins)
    frames[frame_number] << pins
    roll_result(pins)
  end

  def score
    raise BowlingError if unscoreable?
    frames.each.with_index.reduce(SCORE_SEED) do |acc, (frame, index)|
      if standard_frame_strike?(frame)
        acc += strike_bonus(index)
      elsif spare?(frame)
        acc += spare_bonus(index)
      end
      acc + frame.sum
    end
  end

  private

  attr_reader :frames
  attr_accessor :frame_number, :remaining_pins

  def roll_result(pins)
    if standard_frame?
      standard_frame_roll(pins)
    elsif can_roll_next_final_frame_ball?(pins)
      reset_pins
    elsif frame_finished_without_fill_ball?
      game_over
    else
      knock_down(pins)
    end
  end

  def invalid_roll?(pins)
    !pins.between?(MIN_PINS, MAX_PINS) ||
      pins > remaining_pins ||
      frame_number > FINAL_FRAME
  end

  def standard_frame?
    frame_number < FINAL_FRAME
  end

  def standard_frame_roll(pins)
    if all_pins_knocked_down?(pins)
      start_next_frame
    elsif end_of_standard_frame?
      start_next_frame
      reset_pins
    else
      knock_down(pins)
    end
  end

  def all_pins_knocked_down?(pins)
    pins == MAX_PINS
  end

  def end_of_standard_frame?
    frames[frame_number].size == STANDARD_FRAME_SIZE
  end

  def start_next_frame
    self.frame_number = frame_number + FRAME_INCREMENT
  end

  def reset_pins
    self.remaining_pins = MAX_PINS
  end

  def knock_down(pins)
    self.remaining_pins = remaining_pins - pins
  end

  def frame_finished_without_fill_ball?
    frames[frame_number].length == STANDARD_FRAME_SIZE &&
      frames[frame_number].sum < MAX_PINS
  end

  def can_roll_next_final_frame_ball?(pins)
    pins == MAX_PINS ||
      frames[frame_number].first(STANDARD_FRAME_SIZE).sum == MAX_PINS
  end

  def game_over
    self.frame_number = GAME_OVER
  end

  def unscoreable?
    frames.any?(&:empty?) ||
      frames[FINAL_FRAME].length < FINAL_FRAME_SIZE &&
        frames[FINAL_FRAME].sum >= MAX_PINS
  end

  def standard_frame_strike?(frame)
    frame == STANDARD_FRAME_STRIKE
  end

  def strike_bonus(index)
    if before_final_standard_frame?(index)
      if next_standard_frame_is_a_strike?(index)
        double_strike_bonus(index)
      else
        single_strike_bonus(index)
      end
    else
      strike_bonus_from_final_frame(index)
    end
  end

  def before_final_standard_frame?(index)
    index < FINAL_STANDARD_FRAME
  end

  def next_standard_frame_is_a_strike?(index)
    frames[index + FRAME_INCREMENT] == STANDARD_FRAME_STRIKE
  end

  def double_strike_bonus(index)
    next_frame = index + FRAME_INCREMENT
    frames[next_frame].first + frames[next_frame + FRAME_INCREMENT].first
  end

  def single_strike_bonus(index)
    frames[index + FRAME_INCREMENT].sum
  end

  def strike_bonus_from_final_frame(index)
    frames[index + FRAME_INCREMENT].first(STANDARD_FRAME_SIZE).sum
  end

  def spare?(frame)
    frame.sum == MAX_PINS
  end

  def spare_bonus(index)
    frames[index + FRAME_INCREMENT].first
  end
end

module BookKeeping
  VERSION = 3
end
