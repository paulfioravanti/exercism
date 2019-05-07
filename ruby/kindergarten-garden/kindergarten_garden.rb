# frozen_string_literal: true

class Garden
  PLANTS = {
    "C" => :clover,
    "G" => :grass,
    "R" => :radishes,
    "V" => :violets
  }.freeze
  private_constant :PLANTS
  COLUMNS = (0..30).step(2).freeze
  private_constant :COLUMNS
  NEWLINE = "\n"
  private_constant :NEWLINE
  STUDENTS = %i[
    alice
    bob
    charlie
    david
    eve
    fred
    ginny
    harriet
    ileana
    joseph
    kincaid
    larry
    patricia
    roger
    samantha
    xander
  ].freeze
  private_constant :STUDENTS

  def initialize(plants, students = STUDENTS)
    @plants = plants.split(NEWLINE).map(&:chars)
    @students = students.sort
    define_student_methods
  end

  private

  attr_reader :plants, :students

  def define_student_methods
    students.zip(COLUMNS).each do |student, column|
      define_singleton_method(student.downcase) do
        fetch_student_plants(column)
      end
    end
  end

  def fetch_student_plants(column)
    plants.flat_map { |row| row[column..column.succ].map(&PLANTS) }
  end
end
