# frozen_string_literal: true

class Allergies
  ALLERGENS = %w[
    eggs
    peanuts
    shellfish
    strawberries
    tomatoes
    chocolate
    pollen
    cats
  ].freeze
  private_constant :ALLERGENS
  ALLERGEN_SCORE = ->(index) { 1 << index }
  private_constant :ALLERGEN_SCORE

  attr_reader :list

  def initialize(score)
    @list = determine_allogens(score)
  end

  def allergic_to?(item)
    list.include?(item)
  end

  private

  def determine_allogens(score)
    ALLERGENS.select.with_index do |_item, index|
      (score & ALLERGEN_SCORE.call(index)).nonzero?
    end
  end
end
