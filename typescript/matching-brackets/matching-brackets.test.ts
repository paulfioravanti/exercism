import MatchingBrackets from "./matching-brackets"

describe("Matching Brackets", () => {
  it("paired square brackets", () => {
    const matchingBrackets = new MatchingBrackets("[]")
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })

  it("empty string", () => {
    const matchingBrackets = new MatchingBrackets("")
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })

  it("unpaired brackets", () => {
    const matchingBrackets = new MatchingBrackets("[[")
    expect(matchingBrackets.isPaired()).toBeFalsy()
  })

  it("wrong ordered brackets", () => {
    const matchingBrackets = new MatchingBrackets("}{")
    expect(matchingBrackets.isPaired()).toBeFalsy()
  })

  it("wrong closing bracket", () => {
    const matchingBrackets = new MatchingBrackets("{]")
    expect(matchingBrackets.isPaired()).toBeFalsy()
  })

  it("paired with whitespace", () => {
    const matchingBrackets = new MatchingBrackets("{ }")
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })

  it("simple nested brackets", () => {
    const matchingBrackets = new MatchingBrackets("{[]}")
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })

  it("several paired brackets", () => {
    const matchingBrackets = new MatchingBrackets("{}[]")
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })

  it("paired and nested brackets", () => {
    const matchingBrackets = new MatchingBrackets("([{}({}[])])")
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })

  it("unopened closing brackets", () => {
    const matchingBrackets = new MatchingBrackets("{[)][]}")
    expect(matchingBrackets.isPaired()).toBeFalsy()
  })

  it("unpaired and nested brackets", () => {
    const matchingBrackets = new MatchingBrackets("([{])")
    expect(matchingBrackets.isPaired()).toBeFalsy()
  })

  it("paired and wrong nested brackets", () => {
    const matchingBrackets = new MatchingBrackets("[({]})")
    expect(matchingBrackets.isPaired()).toBeFalsy()
  })

  it("math expression", () => {
    const matchingBrackets = new MatchingBrackets(
      "(((185 + 223.85) * 15) - 543)/2"
    )
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })

  it("complex latex expression", () => {
    const matchingBrackets = new MatchingBrackets(
      "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)"
    )
    expect(matchingBrackets.isPaired()).toBeTruthy()
  })
})
