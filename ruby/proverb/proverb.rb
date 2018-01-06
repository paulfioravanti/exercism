# frozen_string_literal: true

class Proverb
  VERSE = "For want of a %<objective>s the %<possession>s was lost."
  private_constant :VERSE
  LAMENT = "And all for the want of a %<objective>s."
  private_constant :LAMENT

  def initialize(*chain, qualifier: nil)
    @chain = chain
    @initial_objective = [qualifier, chain.first].compact.join(" ")
  end

  def to_s
    chain.each_cons(2).reduce([]) do |acc, (objective, possession)|
      acc << format(VERSE, objective: objective, possession: possession)
    end.append(lament).join("\n")
  end

  private

  attr_reader :chain, :initial_objective

  def lament
    format(LAMENT, objective: initial_objective)
  end
end
