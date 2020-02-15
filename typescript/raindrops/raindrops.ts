type RaindropConversion = [number, string]
type RaindropConversions = readonly RaindropConversion[]
type RaindropReducer = (
  acc: string,
  [factor, raindrop]: RaindropConversion
) => string

export default class Raindrops {
  private readonly CONVERSIONS: RaindropConversions = Object.freeze([
    [3, "Pling"],
    [5, "Plang"],
    [7, "Plong"]
  ])

  convert(number: number): string {
    const raindrops: string =
      this.CONVERSIONS.reduce(this.addRaindrop(number), "")
    return raindrops || number.toString()
  }

  private addRaindrop(number: number): RaindropReducer {
    return (acc: string, [factor, raindrop]: RaindropConversion): string => {
      return number % factor === 0 ? acc + raindrop : acc
    }
  }
}
