package ru.msk.sort.comparative.exchange

import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait CombSort[T] extends SortTestCommand[T]{

  // Worst-case performance	O(n^{2})
  // Best-case performance	Teta(n * log n)
  // Average performance	O(n^{2})

  // oriented for Vector
  val shrinkFactor = 1.247

  override def sort(xs: Seq[T]): Seq[T] = {

    @tailrec
    def sortR(ys: Seq[T], gap: Int, firstIndex: Int = 0, wasShuffled: Boolean = false): Seq[T] = gap match {
      case 0 =>
        if (wasShuffled)
          sortR(ys, 1)
        else
          ys
      case _ => {
        val secondIndex = firstIndex + gap

        if (firstIndex < (ys size) && secondIndex < (ys size)) {

          if (ord gt(ys(firstIndex), ys(secondIndex))) {
              val ysSwapped = ys updated(firstIndex, ys(secondIndex)) updated(secondIndex, ys(firstIndex))
              sortR(ysSwapped, gap, firstIndex + 1, true)

          } else
            sortR(ys, gap, firstIndex + 1, wasShuffled || false)

        } else {
          sortR(ys, (gap / shrinkFactor) toInt, 0, wasShuffled)
        }
      }
    }

    sortR(xs toVector, ((xs size) / shrinkFactor) toInt)
  }

}
