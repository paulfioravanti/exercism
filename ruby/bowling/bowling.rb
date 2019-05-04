class Game
  class BowlingError < StandardError; end

  module BowlingRules
    NUM_FRAMES = 10
    START_FRAME = 0
    FRAME_INCREMENT = 1
    MAX_PINS = 10

    FINAL_FRAME = NUM_FRAMES - 1
    private_constant :FINAL_FRAME
    STANDARD_FRAME_SIZE = 2
    private_constant :STANDARD_FRAME_SIZE
    FINAL_FRAME_SIZE = 3
    private_constant :FINAL_FRAME_SIZE
    STANDARD_FRAME_STRIKE = [MAX_PINS].freeze
    private_constant :STANDARD_FRAME_STRIKE

    module Referee
      MIN_PINS = 0
      private_constant :MIN_PINS

      module_function

      def invalid_roll?(frames, frame_number, remaining_pins, pins)
        !pins.between?(MIN_PINS, MAX_PINS) ||
          pins > remaining_pins ||
          frame_number > FINAL_FRAME ||
          frames[FINAL_FRAME].length >= FINAL_FRAME_SIZE
      end

      def unscoreable?(frames)
        frames.any?(&:empty?) ||
          frames[FINAL_FRAME].length < FINAL_FRAME_SIZE &&
            frames[FINAL_FRAME].sum >= MAX_PINS
      end
    end
    private_constant :Referee

    module Scorer
      FINAL_STANDARD_FRAME = FINAL_FRAME - 1
      private_constant :FINAL_STANDARD_FRAME

      module_function

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
      private_class_method :standard_frame_strike?

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
      private_class_method :strike_bonus

      def before_final_standard_frame?(index)
        index < FINAL_STANDARD_FRAME
      end
      private_class_method :before_final_standard_frame?

      def next_standard_frame_is_a_strike?(frames, index)
        frames[index + FRAME_INCREMENT] == STANDARD_FRAME_STRIKE
      end
      private_class_method :next_standard_frame_is_a_strike?

      def double_strike_bonus(frames, index)
        next_frame = index + FRAME_INCREMENT
        frames[next_frame].first + frames[next_frame + FRAME_INCREMENT].first
      end
      private_class_method :double_strike_bonus

      def single_strike_bonus(frames, index)
        frames[index + FRAME_INCREMENT].sum
      end
      private_class_method :single_strike_bonus

      def strike_bonus_from_final_frame(frames, index)
        frames[index + FRAME_INCREMENT].first(STANDARD_FRAME_SIZE).sum
      end
      private_class_method :strike_bonus_from_final_frame

      def spare?(frame)
        frame.sum == MAX_PINS
      end
      private_class_method :spare?

      def spare_bonus(frames, index)
        frames[index + FRAME_INCREMENT].first
      end
      private_class_method :spare_bonus
    end
    private_constant :Scorer

    module_function

    def invalid_roll?(frames, frame_number, remaining_pins, pins)
      Referee.invalid_roll?(frames, frame_number, remaining_pins, pins)
    end

    def unscoreable?(frames)
      Referee.unscoreable?(frames)
    end

    def score(frames, frame, index)
      Scorer.score(frames, frame, index)
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

    def standard_frame?(frame_number)
      frame_number < FINAL_FRAME
    end
    private_class_method :standard_frame?

    def standard_frame_roll(frames, frame_number, pins)
      if all_pins_knocked_down?(pins)
        [:start_next_frame]
      elsif end_of_standard_frame?(frames, frame_number)
        [[:start_next_frame], [:reset_pins]]
      else
        [[:knock_down, pins]]
      end
    end
    private_class_method :standard_frame_roll

    def all_pins_knocked_down?(pins)
      pins == MAX_PINS
    end
    private_class_method :all_pins_knocked_down?

    def end_of_standard_frame?(frames, frame_number)
      frames[frame_number].size == STANDARD_FRAME_SIZE
    end
    private_class_method :end_of_standard_frame?

    def can_roll_next_final_frame_ball?(frames, frame_number, pins)
      pins == MAX_PINS ||
        frames[frame_number].first(STANDARD_FRAME_SIZE).sum == MAX_PINS
    end
    private_class_method :can_roll_next_final_frame_ball?

    def frame_finished_without_fill_ball?(frames, frame_number)
      frames[frame_number].length == STANDARD_FRAME_SIZE &&
        frames[frame_number].sum < MAX_PINS
    end
    private_class_method :frame_finished_without_fill_ball?
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
    if BowlingRules.invalid_roll?(frames, frame_number, remaining_pins, pins)
      raise BowlingError
    end

    frames[frame_number] << pins
    BowlingRules.next_actions(frames, frame_number, pins).each do |action|
      send(*action)
    end
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
