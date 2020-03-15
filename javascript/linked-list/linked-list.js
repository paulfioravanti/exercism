export class LinkedList {
  constructor() {
    this._head = null
  }

  get head() {
    return this._head
  }

  set head(node) {
    this._head = node
  }

  push(value) {
    let head = this.head

    if (!head) {
      this.head = new Node(value)
      return
    }

    const tail = head.prev
    const newTail = new Node(value, { prev: tail, next: head })
    tail.next = newTail
    head.prev = newTail
  }

  pop() {
    let head = this.head
    const tail = head.prev

    if (tail === head) {
      this.head = null
    } else {
      head.prev = tail.prev
      tail.prev.next = head
    }
    return tail.value
  }

  shift() {
    let head = this.head
    let newHead = head.next

    if (newHead === head) {
      this.head = null
    } else {
      newHead.prev = head.prev
      head.prev.next = newHead
      this.head = newHead
    }

    return head.value
  }

  unshift(value) {
    let head = this.head

    if (!head) {
      this.head = new Node(value)
      return
    }

    const newHead = new Node(value, { prev: head.prev, next: head })
    head.prev = newHead
    this.head = newHead
  }

  delete(value) {
    const head = this.head

    if (!head) {
      return
    }

    if (head.value === value) {
      return this.shift()
    }

    let current = head

    while (current.value !== value && current.next !== head) {
      current = current.next
    }

    if (current.value === value) {
      current.prev.next = current.next
      current.next.prev = current.prev
    }
  }

  count() {
    const head = this.head

    if (!head) {
      return 0
    }

    let count = 1
    let current = head

    while (current.next !== head) {
      count += 1
      current = current.next
    }

    return count
  }
}

class Node {
  constructor(value, { prev = this, next = this } = {}) {
    this._value = value
    this._prev = prev
    this._next = next
  }

  get value() {
    return this._value
  }

  get prev() {
    return this._prev
  }

  set prev(node) {
    this._prev = node
  }

  get next() {
    return this._next
  }

  set next(node) {
    this._next= node
  }
}
