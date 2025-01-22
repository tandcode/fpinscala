package fpinscala.exercises.datastructures

import fpinscala.exercises.datastructures.Tree.{Branch, Leaf}

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(value) => 0
    case Branch(left, right) => (left.depth + 1) max (right.depth + 1)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(value) => f(value)
    case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

  def sizeViaFold: Int = fold(_ => 1, (a, b) => 1 + a + b)

  def depthViaFold: Int = fold(_ => 0, (a, b) => (a + 1) max (b + 1))

  def mapViaFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), (a, b) => Branch(a, b))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(value) => value
    case Branch(left, right) => left.maximum max right.maximum

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(v => v, _ max _)
