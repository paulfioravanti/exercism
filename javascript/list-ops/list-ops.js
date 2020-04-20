export class List {
  constructor(values = []) {
    this._values = values
  }

  get values() {
    return this._values
  }

  append(other) {
    const values = this._append(this.values, other.values)
    return new List(values)
  }

  concat(listOfLists) {
    const values = this._concat(this.values, listOfLists.values)
    return new List(values)
  }

  filter(fun) {
    const values = this._filter(fun, [], this.values)
    return new List(values)
  }

  map(fun) {
    const values = this._map(fun, [], this.values)
    return new List(values)
  }

  length() {
    return this._length(this.values)
  }

  foldl(fun, acc) {
    return this._foldl(fun, acc, this.values)
  }

  foldr(fun, acc) {
    return this._foldr(fun, acc, this.values)
  }

  reverse() {
    const values = this._reverse([], this.values)
    return new List(values)
  }

  _append(values1, values2) {
    const values1Empty = values1.length === 0
    const values2Empty = values2.length === 0

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

  _concat(acc, lists) {
    if (lists.length === 0) {
      return acc
    }

    const [head, ...tail] = lists
    return this._concat([...acc, ...head.values], tail)
  }

  _filter(fun, acc, values) {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail] = values
    if (fun(head)) {
      acc = [...acc, head]
    }

    return this._filter(fun, acc, tail)
  }

  _map(fun, acc, values) {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail] = values
    acc = [...acc, fun(head)]
    return this._map(fun, acc, tail)
  }

  _foldl(fun, acc, values) {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail] = values
    acc = fun(acc, head)
    return this._foldl(fun, acc, tail)
  }

  _foldr(fun, acc, values) {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail] = values
    acc = this._foldr(fun, acc, tail)
    return fun(acc, head)
  }

  _length(values) {
    if (values.length === 0) {
      return 0
    }

    // eslint-disable-next-line no-unused-vars
    const [_head, ...tail] = values
    return 1 + this._length(tail)
  }

  _reverse(acc, values) {
    if (values.length === 0) {
      return acc
    }

    const [head, ...tail] = values
    return [...this._reverse(acc, tail), head]
  }
}
