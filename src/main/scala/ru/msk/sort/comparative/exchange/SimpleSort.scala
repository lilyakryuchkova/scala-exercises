package ru.msk.sort.comparative.exchange

import ru.msk.sort.quickcheck.SortTestCommand

import scala.reflect.ClassTag

trait SimpleSort[T] extends SortTestCommand[T]{

  // complexity O(n^3)

  // oriented for List
  override def sort(xs: Seq[T]): Seq[T] = {

    def sortR (ys: List[T], head: List[T]): List[T] = ys match {
      case Nil => head
      case fst :: Nil => head ::: List(fst)
      case fst :: tail =>
        if (ord gt(fst, tail head)) {
          sortR(head ::: (tail head) :: fst :: (tail tail), Nil)
        }
        else
          sortR(tail, head ::: List(fst))
    }

    sortR(xs toList, Nil)
  }


}
