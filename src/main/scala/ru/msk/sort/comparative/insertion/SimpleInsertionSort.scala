package ru.msk.sort.comparative.insertion

import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait SimpleInsertionSort[T] extends SortTestCommand[T]{

  // Worst case performance   О(n2) comparisons, swaps
  // Best-case performance    O(n) comparisons, O(1) swaps
  // Average performance      О(n2) comparisons, swaps
  // Worst-case space complexity	  О(n) total, O(1) auxiliary

  // oriented for List
  override def sort(xs: Seq[T]): Seq[T] = {

    @tailrec
    def insert (sortedTail: Seq[T], element: T, sortedHead: Seq[T]): Seq[T] = sortedTail match {
      case Seq() => sortedHead :+ element
      case Seq(y, ys @ _*) =>
        if (ord lt (y, element)) insert(ys, element, sortedHead :+ y)
        else (sortedHead :+ element) ++ sortedTail
    }

    @tailrec
    def sortR(xs: Seq[T], sorted: Seq[T]): Seq[T] = xs match {
      case Seq() => sorted
      case Seq(x, ys @ _*) =>
        sortR(ys, insert (sorted, x, Nil))
    }

    sortR(xs, Seq())
  }

}
