export class DiffieHellman {
  static get MINIMUM_PRIME() {
    return 2
  }

  constructor(p, g) {
    if (!(this.isPrime(p) && this.isPrime(g))) {
      throw new Error("p and g must be prime numbers")
    }

    this.p = p
    this.g = g
  }

  getPublicKeyFromPrivateKey(privateKey) {
    if (this.isInvalidPrivateKey(privateKey)) {
      throw new Error("invalid private key")
    }

    return this.modPow(this.g, privateKey, this.p)
  }

  getSharedSecret(privateKey, publicKey) {
    return this.modPow(publicKey, privateKey, this.p)
  }

  isPrime(num) {
    const sqrt = Math.sqrt(num)

    for (let i = DiffieHellman.MINIMUM_PRIME; i <= sqrt; i++) {
      if (num % i === 0) {
        return false
      }
    }

    return num > 1
  }

  isInvalidPrivateKey(privateKey) {
    return (
      privateKey < DiffieHellman.MINIMUM_PRIME ||
      privateKey >= this.p
    )
  }

  modPow(base, power, modulus) {
    return base**power % modulus
  }
}
