export class Scale {
  static get SHARP_SCALE() {
    return ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
  }

  static get FLAT_SCALE() {
    return ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]
  }

  static get FLAT_SCALE_TONICS() {
    return ["F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb"]
  }

  static get SEMITONE_STEPS() {
    return { m: 1, M: 2, A: 3 }
  }

  constructor(tonic) {
    this.tonic = this.capitalize(tonic)
    this.scale =
      Scale.FLAT_SCALE_TONICS.includes(tonic)
        ? Scale.FLAT_SCALE
        : Scale.SHARP_SCALE
  }

  chromatic() {
    const scale = this.scale
    const index = scale.indexOf(this.tonic)
    return this.rotate(scale, index)
  }

  interval(intervals) {
    const chromaticScale = this.chromatic()

    return (
      intervals
        .split("")
        .reduce(this.addChromaticScale(chromaticScale), [0, []])[1]
    )
  }

  capitalize(string) {
    return string.charAt(0).toUpperCase() + string.slice(1)
  }

  rotate(list, count) {
    if (count < 1) {
      return list
    }

    const head = list.shift()
    list.push(head)
    return this.rotate(list, count - 1)
  }

  addChromaticScale(chromaticScale) {
    return ([lastIndex, acc], step) => {
      acc.push(chromaticScale[lastIndex])
      lastIndex += Scale.SEMITONE_STEPS[step]
      return [lastIndex, acc]
    }
  }
}
