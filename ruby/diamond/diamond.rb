# frozen_string_literal: true

module Diamond
  DIAMOND_TIP = "A"
  private_constant :DIAMOND_TIP
  NEWLINE = "\n"
  private_constant :NEWLINE
  SPACE = " "
  private_constant :SPACE

  module_function

  def make_diamond(max_letter)
    letters = generate_letters(max_letter)
    max_outer_padding = length_from_center(max_letter)

    letters
      .each
      .with_object([letters.length, max_outer_padding])
      .each_with_object([], &method(:add_line))
      .join
  end

  def generate_letters(max_letter)
    (DIAMOND_TIP..max_letter).to_a + (DIAMOND_TIP...max_letter).to_a.reverse
  end
  private_class_method :generate_letters

  def length_from_center(letter)
    letter.ord - DIAMOND_TIP.ord
  end
  private_class_method :length_from_center

  def add_line((letter, (diamond_height, max_outer_padding)), acc)
    line =
      if letter == DIAMOND_TIP
        diamond_tip(letter, diamond_height)
      else
        diamond_row(letter, max_outer_padding)
      end
    acc << line + NEWLINE
  end
  private_class_method :add_line

  def diamond_tip(letter, diamond_height)
    letter.center(diamond_height)
  end
  private_class_method :diamond_tip

  def diamond_row(letter, max_outer_padding)
    inner_padding = length_from_center(letter)
    # Pad string spacing outwards from center first, then have outer
    # padding fill in the remaining gaps, if any.
    half_row =
      letter
      .ljust(inner_padding)
      .rjust(max_outer_padding)

    half_row + SPACE + half_row.reverse
  end
  private_class_method :diamond_row
end
