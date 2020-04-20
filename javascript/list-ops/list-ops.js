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

  _append(list1, list2) {
    const list1Empty = list1.length === 0
    const list2Empty = list2.length === 0

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

  _concat(acc, listOfLists) {
    if (listOfLists.length === 0) {
      return acc
    }

    const [head, ...tail] = listOfLists
    return this._concat([...acc, ...head.values], tail)
  }

  _filter(fun, acc, list) {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail] = list
    if (fun(head)) {
      acc = [...acc, head]
    }

    return this._filter(fun, acc, tail)
  }

  _map(fun, acc, list) {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail] = list
    acc = [...acc, fun(head)]
    return this._map(fun, acc, tail)
  }

  _foldl(fun, acc, list) {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail] = list
    acc = fun(acc, head)
    return this._foldl(fun, acc, tail)
  }

  _foldr(fun, acc, list) {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail] = list
    acc = this._foldr(fun, acc, tail)
    return fun(acc, head)
  }

  _length(list) {
    if (list.length === 0) {
      return 0
    }

    // eslint-disable-next-line no-unused-vars
    const [_head, ...tail] = list
    return 1 + this._length(tail)
  }

  _reverse(acc, list) {
    if (list.length === 0) {
      return acc
    }

    const [head, ...tail] = list
    return [...this._reverse(acc, tail), head]
  }
}
