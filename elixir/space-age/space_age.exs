defmodule SpaceAge do
  @earth_orbital_period 31_557_600
  @mercury_orbital_factor 0.2408467
  @venus_orbital_factor 0.61519726
  @mars_orbital_factor 1.8808158
  @jupiter_orbital_factor 11.862615
  @saturn_orbital_factor 29.447498
  @uranus_orbital_factor 84.016846
  @neptune_orbital_factor 164.79132

  @type planet ::
          :mercury
          | :venus
          | :earth
          | :mars
          | :jupiter
          | :saturn
          | :uranus
          | :neptune

  @doc """
  Return the number of years a person that has lived for 'seconds' seconds is
  aged on 'planet'.
  """
  @spec age_on(planet, pos_integer) :: float
  def age_on(:earth, seconds) do
    seconds / @earth_orbital_period
  end

  def age_on(:mercury, seconds) do
    age_on(:earth, seconds) / @mercury_orbital_factor
  end

  def age_on(:venus, seconds) do
    age_on(:earth, seconds) / @venus_orbital_factor
  end

  def age_on(:mars, seconds) do
    age_on(:earth, seconds) / @mars_orbital_factor
  end

  def age_on(:jupiter, seconds) do
    age_on(:earth, seconds) / @jupiter_orbital_factor
  end

  def age_on(:saturn, seconds) do
    age_on(:earth, seconds) / @saturn_orbital_factor
  end

  def age_on(:uranus, seconds) do
    age_on(:earth, seconds) / @uranus_orbital_factor
  end

  def age_on(:neptune, seconds) do
    age_on(:earth, seconds) / @neptune_orbital_factor
  end
end
