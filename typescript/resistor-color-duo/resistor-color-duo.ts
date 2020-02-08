type Color =
  | "black"
  | "brown"
  | "red"
  | "orange"
  | "yellow"
  | "green"
  | "blue"
  | "violet"
  | "grey"
  | "white"
type ColorValue = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

export class ResistorColor {
  private readonly MAX_COLORS: number = 2
  private readonly RESISTOR_VALUES: Record<Color, ColorValue> = {
    black: "0",
    brown: "1",
    red: "2",
    orange: "3",
    yellow: "4",
    green: "5",
    blue: "6",
    violet: "7",
    grey: "8",
    white: "9"
  }
  private readonly colors: Color[]

  constructor(colors: Color[]) {
    if (colors.length < this.MAX_COLORS) {
      throw new Error("At least two colors need to be present")
    } else {
      this.colors = colors.slice(0, this.MAX_COLORS)
    }
  }

  value(): number {
    return parseInt(this.colors.reduce(this.addResistorValue, ""))
  }

  private addResistorValue = (acc: string, color: Color): string => {
    return acc + this.RESISTOR_VALUES[color]
  }
}
