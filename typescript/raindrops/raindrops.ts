type RaindropConversion = [number, string]
type RaindropConversions = Readonly<RaindropConversion[]>
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

  convert(num: number): string {
    const raindrops: string = this.CONVERSIONS.reduce(this.addRaindrop(num), "")
    return raindrops || num.toString()
  }

  private addRaindrop(num: number): RaindropReducer {
    return (acc: string, [factor, raindrop]: RaindropConversion): string => {
      return num % factor === 0 ? acc + raindrop : acc
    }
  }
}
