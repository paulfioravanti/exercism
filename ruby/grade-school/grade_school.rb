class School
  def initialize
    @enrollments = Hash.new { |hash, key| hash[key] = [] }
  end

  def add(name, grade)
    enrollments[grade] << name
  end

  def students(grade)
    enrollments[grade].sort
  end

  def students_by_grade
    enrollments.sort.map do |grade, students|
      { grade: grade, students: students.sort }
    end
  end

  private

  attr_reader :enrollments
end

module BookKeeping
  VERSION = 3
end
