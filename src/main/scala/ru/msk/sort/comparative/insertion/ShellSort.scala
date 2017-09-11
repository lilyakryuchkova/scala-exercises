package ru.msk.sort.comparative.insertion

import ru.msk.sort.comparative.insertion.shellgaps.Gap
import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait ShellSort[T] extends SortTestCommand[T]{

  // Worst case performance   О(n2) comparisons, swaps
  // Best-case performance    O(n) comparisons, O(1) swaps
  // Average performance      О(n2) comparisons, swaps
  // Worst-case space complexity	  О(n) total, O(1) auxiliary

  val gapsGenerator: Gap

  // oriented for Vector
  override def sort(xs: Seq[T]): Seq[T] = {

    object Direction extends Enumeration {
      type Direction = Value
      val forward, backward = Value
    }

    import Direction._

    def getNextIndex(index: Int, gap: Int, dir: Direction): Int =
      if (dir == Direction.forward) index + gap
      else index - gap

    @tailrec
    def helper(ts: Seq[T], iterations: Int, elementIndex: Int, gap: Int): Seq[T] = {

      @tailrec
      def insert (ys: Seq[T], index: Int, element: T): Seq[T] = {

        def update (ls: Seq[T], ind: Int, newValue: T): Seq[T] =
          if (ord eq(newValue, ls(ind)))
            ls
          else
            ls updated (ind, newValue)

        if (index < (ys size)) {
          val compareIndex = getNextIndex(index, gap, Direction.backward)
          if (compareIndex > -1) {
            if (ord gt(ys(compareIndex), element)) {
              val ysSwapped = update(ys, index, ys(compareIndex))
              insert(ysSwapped, compareIndex, element)
            } else
              update(ys, index, element)
          } else update(ys, index, element)
        } else ys
      }

      if (elementIndex < (ts size) ) {
        val nextElementIndex = getNextIndex(elementIndex, gap, Direction.forward)
        if (nextElementIndex < (ts size)) {
          helper(insert(ts, nextElementIndex, ts(nextElementIndex)), iterations, nextElementIndex, gap)
        } else {
          val nextArrayIndex = iterations - 1
          if (nextArrayIndex > 0)
            helper(ts, nextArrayIndex, gap - nextArrayIndex, gap)
          else
            ts
        }
      } else ts
    }

    @tailrec
    def sortR(xs: Seq[T], gaps: Seq[Int]): Seq[T] = gaps match {
      case Seq() => xs
      case Seq(gap, otherGap @ _*) => {
        sortR(helper(xs, gap, 0, gap), otherGap)
      }
    }

    sortR(xs toVector, gapsGenerator gaps(xs size))
  }

}
