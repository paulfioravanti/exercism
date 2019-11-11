defmodule DndCharacter do
  @type t :: %__MODULE__{
          strength: pos_integer(),
          dexterity: pos_integer(),
          constitution: pos_integer(),
          intelligence: pos_integer(),
          wisdom: pos_integer(),
          charisma: pos_integer(),
          hitpoints: pos_integer()
        }

  defstruct ~w[
    strength
    dexterity
    constitution
    intelligence
    wisdom
    charisma
    hitpoints
  ]a

  @die_faces 1..6
  @rolls 0..3
  @initial_hitpoints 10

  @spec modifier(pos_integer()) :: integer()
  def modifier(score), do: Integer.floor_div(score - 10, 2)

  @spec ability :: pos_integer()
  def ability do
    rolls = for _roll <- @rolls, do: Enum.random(@die_faces)
    Enum.sum(rolls) - Enum.min(rolls)
  end

  @spec character :: t()
  def character do
    constitution = ability()

    %DndCharacter{
      strength: ability(),
      dexterity: ability(),
      constitution: constitution,
      intelligence: ability(),
      wisdom: ability(),
      charisma: ability(),
      hitpoints: @initial_hitpoints + modifier(constitution)
    }
  end
end
