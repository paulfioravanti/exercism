type MaybeError<T> = T | never

export default class DiffieHellman {
  private static readonly MINIMUM_PRIME: number = 2
  private readonly p: number
  private readonly g: number

  constructor(p: number, g: number) {
    if (!(this.isPrime(p) && this.isPrime(g))) {
      throw new Error("p and g must be prime numbers")
    }

    this.p = p
    this.g = g
  }

  getPublicKeyFromPrivateKey(privateKey: number): MaybeError<number> {
    if (this.isInvalidPrivateKey(privateKey)) {
      throw new Error("invalid private key")
    }

    return this.modPow(this.g, privateKey, this.p)
  }

  getSharedSecret(privateKey: number, publicKey: number): number {
    return this.modPow(publicKey, privateKey, this.p)
  }

  private isPrime(num: number): boolean {
    const sqrt: number = Math.sqrt(num)

    for (let i: number = DiffieHellman.MINIMUM_PRIME; i <= sqrt; i++) {
      if (num % i === 0) {
        return false
      }
    }

    return num > 1
  }

  private isInvalidPrivateKey(privateKey: number): boolean {
    return (
      privateKey < DiffieHellman.MINIMUM_PRIME ||
      privateKey >= this.p
    )
  }

  private modPow(base: number, power: number, modulus: number): number {
    return base**power % modulus
  }
}
