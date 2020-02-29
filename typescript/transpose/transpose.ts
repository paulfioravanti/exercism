type CharacterReducer =
  (acc: string[], character: string, index: number) => string[]

export default abstract class Transpose {
  static transpose(rows: string[]): string[] {
    return rows.reduce(Transpose.transposeRow, [])
  }

  private static transposeRow(
    acc: string[],
    row: string,
    rowIndex: number
  ): string[] {
    return (
      row
        .split("")
        .reduce(Transpose.transformValue(rowIndex), acc)
    )
  }

  private static transformValue(rowIndex: number): CharacterReducer {
    return (acc: string[], character: string, index: number): string[] => {
      if (acc[index] === undefined) {
        acc[index] = " ".repeat(rowIndex)
      }
      acc[index] += character

      return acc
    }
  }
}
