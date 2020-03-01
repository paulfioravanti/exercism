const MIN_SCORE = 3
const MAX_SCORE = 18

export const abilityModifier = score => {
  if (score < MIN_SCORE) {
    throw new Error("Ability scores must be at least 3")
  }

  if (score > MAX_SCORE) {
    throw new Error("Ability scores can be at most 18")
  }

  return Math.floor((score - 10) / 2)
}

export class Character {
  static get MIN_DIE_FACE() {
    return 1
  }

  static get MAX_DIE_FACE() {
    return 6
  }

  static get ROLLS() {
    return [...Array(4).keys()]
  }

  static get INITIAL_HITPOINTS() {
    return 10
  }

  static rollAbility() {
    const rolls = Character.ROLLS.map(Character.roll)
    return Character.sum(rolls) - Math.min(...rolls)
  }

  static sum(rolls) {
    return rolls.reduce((acc, roll) => acc + roll, 0)
  }

  static roll() {
    return (
      Math.floor(
        Math.random() * (Character.MAX_DIE_FACE - Character.MIN_DIE_FACE + 1)
      ) + Character.MIN_DIE_FACE
    )
  }

  constructor() {
    this._strength = Character.rollAbility()
    this._dexterity = Character.rollAbility()
    this._constitution = Character.rollAbility()
    this._intelligence = Character.rollAbility()
    this._wisdom = Character.rollAbility()
    this._charisma = Character.rollAbility()
    this._hitpoints =
      Character.INITIAL_HITPOINTS + abilityModifier(this.constitution)
  }

  get strength() {
    return this._strength
  }

  get dexterity() {
    return this._dexterity
  }

  get constitution() {
    return this._constitution
  }

  get intelligence() {
    return this._intelligence
  }

  get wisdom() {
    return this._wisdom
  }

  get charisma() {
    return this._charisma
  }

  get hitpoints() {
    return this._hitpoints
  }
}
