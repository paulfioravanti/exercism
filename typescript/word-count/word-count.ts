type WordTally = Map<string, number>
type Maybe<T> = T | undefined

export default class Words {
  private readonly WORD: RegExp = /\s+/

  count(sentence: string): WordTally {
    const words: string[] = this.convertToWords(sentence)
    return words.reduce(this.incrementTallyForWord, new Map())
  }

  private convertToWords(sentence: string): string[] {
    return sentence.trim().toLowerCase().split(this.WORD)
  }

  private incrementTallyForWord(acc: WordTally, word: string): WordTally {
    const tallyValue: Maybe<number> = acc.get(word)
    acc.set(word, tallyValue ? tallyValue + 1 : 1)
    return acc
  }
}
