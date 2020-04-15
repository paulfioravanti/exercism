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

  private _append(list1: T[], list2: T[]): T[] {
    const list1Empty: boolean = list1.length === 0
    const list2Empty: boolean = list2.length === 0

    if (list1Empty) {
      if (list2Empty) {
        return []
      }
      return list2
    } else if (list2Empty) {
      return list1
    } else {
      return [...list1, ...list2]
    }
  }

  private _concat(acc: T[], listOfLists: List<T>[]): T[] {
    if (listOfLists.length === 0) {
      return acc
    }

    const [head, ...tail]: List<T>[] = listOfLists
    return this._concat([...acc, ...head.values], tail)
  }

  private _filter(fun: FilterFunction<T>, acc: T[], list: T[]): T[] {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = list
    if (fun.apply(null, [head])) {
      acc = [...acc, head]
    }

    return this._filter(fun, acc, tail)
  }

  private _map(fun: MapFunction<T>, acc: T[], list: T[]): T[] {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = list
    acc = [...acc, fun.apply(null, [head])]
    return this._map(fun, acc, tail)
  }

  private _length(list: T[]): number {
    if (list.length === 0) {
      return 0
    }

    const [_head, ...tail]: T[] = list
    return 1 + this._length(tail)
  }

  private _foldl(fun: FoldFunction<T>, acc: T, list: T[]): T {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = list
    acc = fun.apply(null, [acc, head])
    return this._foldl(fun, acc, tail)
  }

  private _foldr(fun: FoldFunction<T>, acc: T, list: T[]): T {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = list
    acc = this._foldr(fun, acc, tail)
    return fun.apply(null, [acc, head])
  }

  private _reverse(acc: T[], list: T[]): T[] {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail]: T[] = list
    return [...this._reverse(acc, tail), head]
  }
}
