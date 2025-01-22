package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  // should be non-implemented
  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => emptyListException("tail")
    case Cons(_, tail) => tail

  private def emptyListException(op: String): Nothing =
    throw new IllegalStateException(s"Illegal operation for empty list: $op")

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => emptyListException("setHead")
    case Cons(_, tail) => Cons(h, tail)

  def drop[A](l: List[A], n: Int): List[A] = if n <= 0 then l else l match
    case Nil => l
    case Cons(_, tail) => drop(tail, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Nil => l
    case Cons(head, tail) => if f(head) then dropWhile(tail, f) else l

  def init[A](l: List[A]): List[A] = l match
    case Nil => emptyListException("init")
    case Cons(head, tail) => if tail == Nil then Nil else Cons(head, init(tail))

  def length[A](l: List[A]): Int = foldRight(l, 0, { case(_, acc) => acc + 1 })

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (ac, _) => ac + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil, (acc: List[A], h: A) => Cons(h, acc))

  def foldRightViaFoldLeft[A,B](ls: List[A], acc: B, f: (A, B) => B): B = foldLeft(reverse(ls), acc, (b, a) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, (h, acc) => Cons(h, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil, (ll: List[A], acc: List[A]) => appendViaFoldRight(ll, acc))

  def incrementEach(l: List[Int]): List[Int] = foldRight(l, Nil, (h: Int, acc: List[Int]) => Cons(h + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil, (h: Double, acc: List[String]) => Cons(h.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRight(l, Nil, (h: A, acc: List[B]) => Cons(f(h), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil, (h: A, acc: List[A]) => if f(h) then Cons(h, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil, (h: A, acc: List[B]) => append(f(h), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = zip(a, b, _ + _)

  def zip[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    @tailrec
    def zipInner(l1: List[A], l2: List[B], r: List[C]): List[C] =
      (l1, l2) match
        case (Nil, _) | (_, Nil) => r
        case (Cons(h1, t1), Cons(h2, t2)) => zipInner(t1, t2, Cons(f(h1, h2), r))

    reverse(zipInner(a, b, Nil))

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(_, tail), _) => if isSubsequence(sup, sub) then true else hasSubsequence(tail, sub)

  @tailrec
  private def isSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if h1 == h2 then isSubsequence(t1, t2) else false

