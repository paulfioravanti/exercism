class Game
  class BowlingError < StandardError; end

  module BowlingRules
    NUM_FRAMES = 10
    START_FRAME = 0
    FRAME_INCREMENT = 1

    FINAL_FRAME = 9
    private_constant :FINAL_FRAME
    MIN_PINS = 0
    private_constant :MIN_PINS
    MAX_PINS = 10
    STANDARD_FRAME_SIZE = 2
    private_constant :STANDARD_FRAME_SIZE
    FINAL_FRAME_SIZE = 3
    private_constant :FINAL_FRAME_SIZE
    STANDARD_FRAME_STRIKE = [MAX_PINS].freeze
    private_constant :STANDARD_FRAME_STRIKE
    FINAL_STANDARD_FRAME = FINAL_FRAME - 1
    private_constant :FINAL_STANDARD_FRAME

    module_function

    def invalid_roll?(frame_number, pins, remaining_pins)
      !pins.between?(MIN_PINS, MAX_PINS) ||
        pins > remaining_pins ||
        frame_number > FINAL_FRAME
    end

    def unscoreable?(frames)
      frames.any?(&:empty?) ||
        frames[FINAL_FRAME].length < FINAL_FRAME_SIZE &&
          frames[FINAL_FRAME].sum >= MAX_PINS
    end

    def next_actions(frames, frame_number, pins)
      if standard_frame?(frame_number)
        standard_frame_roll(frames, frame_number, pins)
      elsif can_roll_next_final_frame_ball?(frames, frame_number, pins)
        [:reset_pins]
      elsif frame_finished_without_fill_ball?(frames, frame_number)
        [:game_over]
      else
        [[:knock_down, pins]]
      end
    end

    def score(frames, frame, index)
      score = 0
      if standard_frame_strike?(frame)
        score += strike_bonus(frames, index)
      elsif spare?(frame)
        score += spare_bonus(frames, index)
      end
      score + frame.sum
    end

    def standard_frame_strike?(frame)
      frame == STANDARD_FRAME_STRIKE
    end

    def strike_bonus(frames, index)
      if before_final_standard_frame?(index)
        if next_standard_frame_is_a_strike?(frames, index)
          double_strike_bonus(frames, index)
        else
          single_strike_bonus(frames, index)
        end
      else
        strike_bonus_from_final_frame(frames, index)
      end
    end

    def before_final_standard_frame?(index)
      index < FINAL_STANDARD_FRAME
    end

    def standard_frame?(frame_number)
      frame_number < FINAL_FRAME
    end

    def next_standard_frame_is_a_strike?(frames, index)
      frames[index + FRAME_INCREMENT] == STANDARD_FRAME_STRIKE
    end

    def double_strike_bonus(frames, index)
      next_frame = index + FRAME_INCREMENT
      frames[next_frame].first + frames[next_frame + FRAME_INCREMENT].first
    end

    def single_strike_bonus(frames, index)
      frames[index + FRAME_INCREMENT].sum
    end

    def strike_bonus_from_final_frame(frames, index)
      frames[index + FRAME_INCREMENT].first(STANDARD_FRAME_SIZE).sum
    end

    def spare?(frame)
      frame.sum == MAX_PINS
    end

    def spare_bonus(frames, index)
      frames[index + FRAME_INCREMENT].first
    end

    def standard_frame_roll(frames, frame_number, pins)
      if all_pins_knocked_down?(pins)
        [:start_next_frame]
      elsif end_of_standard_frame?(frames, frame_number)
        [[:start_next_frame], [:reset_pins]]
      else
        [[:knock_down, pins]]
      end
    end

    def all_pins_knocked_down?(pins)
      pins == MAX_PINS
    end

    def end_of_standard_frame?(frames, frame_number)
      frames[frame_number].size == STANDARD_FRAME_SIZE
    end

    def can_roll_next_final_frame_ball?(frames, frame_number, pins)
      pins == MAX_PINS ||
        frames[frame_number].first(STANDARD_FRAME_SIZE).sum == MAX_PINS
    end

    def frame_finished_without_fill_ball?(frames, frame_number)
      frames[frame_number].length == STANDARD_FRAME_SIZE &&
        frames[frame_number].sum < MAX_PINS
    end
  end
  private_constant :BowlingRules

  GAME_OVER = 10
  private_constant :GAME_OVER

  def initialize
    @frames = Array.new(BowlingRules::NUM_FRAMES) { [] }
    @frame_number = BowlingRules::START_FRAME
    @remaining_pins = BowlingRules::MAX_PINS
  end

  def roll(pins)
    if BowlingRules.invalid_roll?(frame_number, pins, remaining_pins)
      raise BowlingError
    end
    frames[frame_number] << pins
    roll_result(pins)
  end

  def score
    raise BowlingError if BowlingRules.unscoreable?(frames)
    frames.each.with_index.reduce(0) do |acc, (frame, index)|
      acc + BowlingRules.score(frames, frame, index)
    end
  end

  private

  attr_reader :frames
  attr_accessor :frame_number, :remaining_pins

  def roll_result(pins)
    BowlingRules.next_actions(frames, frame_number, pins).each do |action|
      send(*action)
    end
  end

  def start_next_frame
    self.frame_number = frame_number + BowlingRules::FRAME_INCREMENT
  end

  def reset_pins
    self.remaining_pins = BowlingRules::MAX_PINS
  end

  def knock_down(pins)
    self.remaining_pins = remaining_pins - pins
  end

  def game_over
    self.frame_number = GAME_OVER
  end
end

module BookKeeping
  VERSION = 3
end
