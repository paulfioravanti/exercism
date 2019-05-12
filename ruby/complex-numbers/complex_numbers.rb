class ComplexNumber
  ADD_POWERS_OF_TWO = lambda do |complex_number|
    complex_number.real**2 + complex_number.imaginary**2
  end
  private_constant :ADD_POWERS_OF_TWO

  attr_reader :real, :imaginary

  def initialize(real, imaginary)
    @real = real
    @imaginary = imaginary
  end

  def ==(other)
    real == other.real && imaginary == other.imaginary
  end

  # (a + i * b) + (c + i * d) = (a + c) + (b + d) * i
  def +(other)
    self.class.new(
      add_real(other),
      add_imaginary(other)
    )
  end

  # (a + i * b) - (c + i * d) = (a - c) + (b - d) * i
  def -(other)
    self.class.new(
      subtract_real(other),
      subtract_imaginary(other)
    )
  end

  # (a + i * b) * (c + i * d) = (a * c - b * d) + (b * c + a * d) * i
  def *(other)
    self.class.new(
      multiplication_real(other),
      multiplication_imaginary(other)
    )
  end

  # (a + i * b) / (c + i * d) =
  #   (a * c + b * d) / (c^2 + d^2) + (b * c - a * d) / (c^2 + d^2) * i
  def /(other)
    self.class.new(
      divide_real(other),
      divide_imaginary(other)
    )
  end

  # |z| = sqrt(a^2 + b^2)
  def abs
    self
      .then(&ADD_POWERS_OF_TWO)
      .then(&Math.method(:sqrt))
  end

  # a - b * i
  def conjugate
    self.class.new(real, -imaginary)
  end

  # e^(a + i * b) = e^a * e^(i * b)
  # => e^(i * b) = cos(b) + i * sin(b)
  def exp
    self.class.new(
      Math.exp(real) * eulers_formula,
      0
    )
  end

  private

  # (a + c)
  def add_real(other)
    real + other.real
  end

  # (b + d)
  def add_imaginary(other)
    imaginary + other.imaginary
  end

  # (a - c)
  def subtract_real(other)
    real - other.real
  end

  # (b - d)
  def subtract_imaginary(other)
    imaginary - other.imaginary
  end

  # (a * c - b * d)
  def multiplication_real(other)
    multiply_real(other) - multiply_imaginary(other)
  end

  # (b * c + a * d)
  def multiplication_imaginary(other)
    multiply_imaginary_to_real(other) + multiply_real_to_imaginary(other)
  end

  # (a * c)
  def multiply_real(other)
    real * other.real
  end

  # (a * d)
  def multiply_real_to_imaginary(other)
    real * other.imaginary
  end

  # (b * d)
  def multiply_imaginary(other)
    imaginary * other.imaginary
  end

  # (b * c)
  def multiply_imaginary_to_real(other)
    imaginary * other.real
  end

  # (a * c + b * d) / (c^2 + d^2)
  def divide_real(other)
    (multiply_real(other) + multiply_imaginary(other)) /
      division_denominator(other)
  end

  # (b * c - a * d) / (c^2 + d^2)
  def divide_imaginary(other)
    (multiply_imaginary_to_real(other) - multiply_real_to_imaginary(other)) /
      division_denominator(other)
  end

  # (c^2 + d^2)
  def division_denominator(other)
    other
      .then(&ADD_POWERS_OF_TWO)
      .to_f
  end

  # e^(i * b) = cos(b) + i * sin(b)
  def eulers_formula
    (Math.cos(imaginary) + Math.sin(imaginary)).round
  end
end
