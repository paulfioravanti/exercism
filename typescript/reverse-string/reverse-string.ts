export default class ReverseString {
  static reverse(text: string): string {
    return text.split("").reverse().join("")
  }
}
