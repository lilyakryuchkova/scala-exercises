package ru.msk.sort.comparative.exchange.quicksort

import scala.annotation.tailrec

trait Partition[T] {

  val pivot: Pivot[T]
  implicit val ord: Ordering[T]

  def doIt (xs: Seq[T], lo: Int, hi: Int): (Seq[T], (Int,Int))

}

final class Hoare[T](val pivot: Pivot[T])(implicit val ord: Ordering[T]) extends Partition[T] {

  object Direction extends Enumeration {
    type Direction = Value
    val forward, backward = Value
  }

  import Direction._

  def doIt (xs: Seq[T], lo: Int, hi: Int): (Seq[T], (Int,Int)) = {

    def ordFunction(dir: Direction): (T,T) => Boolean =
      if (dir == Direction.forward) ord lt
      else ord gt

    def nextIndexFunction(dir: Direction): (Int) => Int =
      if (dir == Direction.backward) (a) => a - 1
      else (a) => a + 1

    def nextIndex(dir: Direction, index: Int): Int =
      if (dir == Direction.backward) index - 1
      else index + 1

    val pivotIndex = pivot get(xs, lo, hi)
    val pivotElement = xs(pivotIndex)
    /* Helper for implement cycles:
    *   do
            i := i + 1
        while A[i] < pivot
     and
        do
            j := j - 1
        while A[j] > pivot
    */
    @tailrec
    def helper (ys: Seq[T], index: Int, dir: Direction): Int = {
      if (ordFunction(dir)(ys(index), pivotElement))
        helper(ys, nextIndex(dir, index), dir)
      else
        index
    }

    @tailrec
    def outerCycle(ys: Seq[T], i: Int = lo, j: Int = hi): (Seq[T], (Int,Int)) = {
      val left = helper(ys, i, Direction.forward)
      val right = helper(ys, j, Direction.backward)

      if (left < right)
        if (ord gt (ys(left), ys(right)))
          outerCycle(ys updated(left, ys(right)) updated(right, ys(left)), left +1, right -1)
        else
          outerCycle(ys , left +1, right -1)
      else
        (ys, (right, left))
    }

    outerCycle(xs)

  }

}

/*
* Dutch national flag problem
* */
final class ThreeWay[T](val pivot: Pivot[T])(implicit val ord: Ordering[T]) extends Partition[T] {

  def doIt (xs: Seq[T], lo: Int, hi: Int): (Seq[T], (Int,Int)) = {

    val pivotIndex = pivot get(xs, lo, hi)
    val pivotElement = xs(pivotIndex)

    @tailrec
    def helper(ys: Seq[T], left: Int = lo, j: Int = lo, right: Int = hi): (Seq[T], (Int, Int)) = {
      if (j > right)
        (ys, (left, right))
      else {
        if (ord lt(ys(j), pivotElement)) {
          helper(ys updated(left, ys(j)) updated(j, ys(left)), left + 1, j + 1, right)
        } else if (ord gt(ys(j), pivotElement))
          if (ord lt(ys(right), ys(j)))
            helper(ys updated(right, ys(j)) updated(j, ys(right)), left, j, right - 1)
          else
            helper(ys, left, j, right - 1)
        else
          helper(ys, left, j + 1, right)
      }
    }

    helper(xs)

  }
}