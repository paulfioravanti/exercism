type MaybeNull<T> = T | null
type MaybeVoid<T> = T | void
type Links<T> = { prev: Node<T>, next: Node<T> } | {}
type HeadNode<T> = MaybeNull<Node<T>>

class Node<T> {
  readonly value: T
  prev: Node<T>
  next: Node<T>

  constructor(value: T, links: Links<T> = {}) {
    this.value = value
    this.prev = ("prev" in links) ? links.prev : this
    this.next = ("next" in links) ? links.next : this
  }
}

export default class LinkedList<T> {
  private head: HeadNode<T>

  constructor() {
    this.head = null
  }

  push(value: T): void {
    const head: HeadNode<T> = this.head

    if (!head) {
      this.head = new Node(value)
      return
    }

    const tail: Node<T> = head.prev
    const newTail: Node<T> = new Node(value, { prev: tail, next: head })
    tail.next = newTail
    head.prev = newTail
  }

  pop(): MaybeVoid<T> {
    const head: HeadNode<T> = this.head

    if (!head) {
      return
    }

    const tail: Node<T> = head.prev

    if (tail === head) {
      this.head = null
    } else {
      head.prev = tail.prev
      tail.prev.next = head
    }
    return tail.value
  }

  shift(): MaybeVoid<T> {
    const head: HeadNode<T> = this.head

    if (!head) {
      return
    }

    const newHead: Node<T> = head.next

    if (newHead === head) {
      this.head = null
    } else {
      newHead.prev = head.prev
      head.prev.next = newHead
      this.head = newHead
    }

    return head.value
  }

  unshift(value: T): void {
    const head: HeadNode<T> = this.head

    if (!head) {
      this.head = new Node(value)
      return
    }

    const newHead: Node<T> = new Node(value, { prev: head.prev, next: head })
    head.prev = newHead
    this.head = newHead
  }

  count(): number {
    const head: HeadNode<T> = this.head

    if (!head) {
      return 0
    }

    let count = 1
    let current: Node<T> = head

    while (current.next !== head) {
      count += 1
      current = current.next
    }

    return count
  }

  delete(value: T): void {
    const head: HeadNode<T> = this.head

    if (!head) {
      return
    }

    if (head.value === value) {
      this.shift()
      return
    }

    let current: Node<T> = head

    while (current.value !== value && current.next !== head) {
      current = current.next
    }

    if (current.value === value) {
      current.prev.next = current.next
      current.next.prev = current.prev
    }
  }
}
