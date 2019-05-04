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
    enrollments.sort.map(&method(:enrollment_to_grade))
  end

  private

  attr_reader :enrollments

  def enrollment_to_grade((grade, students))
    { grade: grade, students: students.sort }
  end
end
