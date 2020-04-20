type FilterFunction<T> = (el: T) => boolean
type MapFunction<T> = (el: T) => T
type FoldFunction<T> = (acc: T, el: T) => T

export default class List<T = number> {
  readonly values: T[]

  constructor(values: T[] = []) {
    this.values = values
  }

  append(other: List<T>): List<T> {
    const values: T[] = this._append(this.values, other.values)
    return new List(values)
  }

  concat(listOfLists: List<List<T>>): List<T> {
    const values: T[] = this._concat(this.values, listOfLists.values)
    return new List(values)
  }

  filter(fun: FilterFunction<T>): List<T> {
    const values: T[] = this._filter(fun, [], this.values)
    return new List(values)
  }

  map(fun: MapFunction<T>): List<T> {
    const values: T[] = this._map(fun, [], this.values)
    return new List(values)
  }

  length(): number {
    return this._length(this.values)
  }

  foldl(fun: FoldFunction<T>, acc: T): T {
    return this._foldl(fun, acc, this.values)
  }

  foldr(fun: FoldFunction<T>, acc: T): T {
    return this._foldr(fun, acc, this.values)
  }

  reverse(): List<T> {
    const values: T[] = this._reverse([], this.values)
    return new List(values)
  }

  private _append(values1: T[], values2: T[]): T[] {
    const values1Empty: boolean = values1.length === 0
    const values2Empty: boolean = values2.length === 0

    if (values1Empty) {
      if (values2Empty) {
        return []
      }
      return values2
    } else if (values2Empty) {
      return values1
    } else {
      return [...values1, ...values2]
    }
  }

  private _concat(acc: T[], lists: List<T>[]): T[] {
    if (lists.length === 0) {
      return acc
    }

    const [head, ...tail]: List<T>[] = lists
    return this._concat([...acc, ...head.values], tail)
  }

  private _filter(fun: FilterFunction<T>, acc: T[], values: T[]): T[] {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = values
    if (fun(head)) {
      acc = [...acc, head]
    }

    return this._filter(fun, acc, tail)
  }

  private _map(fun: MapFunction<T>, acc: T[], values: T[]): T[] {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = values
    acc = [...acc, fun(head)]
    return this._map(fun, acc, tail)
  }

  private _length(values: T[]): number {
    if (values.length === 0) {
      return 0
    }

    const [_head, ...tail]: T[] = values
    return 1 + this._length(tail)
  }

  private _foldl(fun: FoldFunction<T>, acc: T, values: T[]): T {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = values
    acc = fun(acc, head)
    return this._foldl(fun, acc, tail)
  }

  private _foldr(fun: FoldFunction<T>, acc: T, values: T[]): T {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = values
    acc = this._foldr(fun, acc, tail)
    return fun(acc, head)
  }

  private _reverse(acc: T[], values: T[]): T[] {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = values
    return [...this._reverse(acc, tail), head]
  }
}
