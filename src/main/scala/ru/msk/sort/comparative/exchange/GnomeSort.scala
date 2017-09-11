package ru.msk.sort.comparative.exchange

import ru.msk.sort.quickcheck.SortTestCommand

trait GnomeSort[T] extends SortTestCommand[T]{

  // Worst-case performance	O(n^{2})
  // Best-case performance	O(n)
  // Average performance	O(n^{2})

  // oriented for Vector
  override def sort(xs: Seq[T]): Seq[T] = {

    def sortR(ys: Seq[T], firstIndex: Int, lastSawIndex: Int = -1, towards: Boolean = true): Seq[T] = firstIndex match {
      case -1 => sortR(ys, lastSawIndex)
      case _ => {
        val secondIndex = firstIndex + 1

        if (firstIndex >= (ys size) || secondIndex >= (ys size)) {
          ys
        } else if (ord gt (ys(firstIndex), ys(secondIndex))) {
          val ysSwaped = ys updated (firstIndex, ys(secondIndex)) updated (secondIndex, ys(firstIndex))
          sortR(ysSwaped, firstIndex - 1, secondIndex, false)
        } else if (towards)
          sortR(ys, secondIndex)
        else
          sortR(ys, lastSawIndex)
      }
    }

    sortR(xs toVector, 0)
  }

}
