type LegacyData = Record<string, string[]>
type LegacyEntry = [string, string[]]
type NewData = Record<string, number>
type LegacyEntryReducer = (acc: NewData, letter: string) => NewData

const assignPointValue = (pointValue: string): LegacyEntryReducer => {
  return (acc: NewData, letter: string): NewData => {
    acc[letter.toLowerCase()] = parseInt(pointValue)
    return acc
  }
}

const transformLetters = (
  acc: NewData,
  [pointValue, letters]: LegacyEntry
  ): NewData => {
  return letters.reduce(assignPointValue(pointValue), acc)
}

export default function transform(scores: LegacyData): NewData {
  return Object.entries(scores).reduce(transformLetters, {})
}
