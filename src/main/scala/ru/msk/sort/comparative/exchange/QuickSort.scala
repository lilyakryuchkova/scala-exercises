package ru.msk.sort.comparative.exchange

import ru.msk.sort.comparative.exchange.quicksort.Partition
import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait QuickSort[T] extends SortTestCommand[T]{

  // Worst-case performance	O(n^{2})
  // Best-case performance
  //    O(n * log n) - simple partition
  //    O(n) - three way partion and equal keys
  // Average performance	O(n * log n)

  val partition: Partition[T]

  override def sort(xs: Seq[T]): Seq[T] = {

    @tailrec
    def quicksort(xs: Seq[T], tasks: Seq[QuickSortSubArray]): Seq[T] = tasks match {
      case Seq() => xs
      case Seq(firstTask, others @ _*) => {

        val (sortedPartially, (left, right)) = partition doIt(xs, firstTask lo, firstTask hi)
        quicksort(sortedPartially, others ++ (firstTask split(left, right)))
      }
    }

    quicksort(xs toVector, Seq(new QuickSortSubArray(0, (xs size)-1 )))
  }

  sealed class QuickSortSubArray (val lo: Int, val hi: Int) {

    def canBeSplitted: Boolean = (hi - lo >= 1)

    def split(left: Int, right: Int): Seq[QuickSortSubArray] = {
      if (canBeSplitted) {
        if (lo < left && right < hi)
          Seq(new QuickSortSubArray(lo, left), new QuickSortSubArray(right, hi))
        else if (lo < left && left < hi)
          Seq(new QuickSortSubArray(lo, left))
        else if (right < hi && right > lo)
          Seq(new QuickSortSubArray(right, hi))
        else
          Seq()
      } else
        Seq[QuickSortSubArray]()
    }
  }
}
