package ru.msk.sort.comparative.exchange

import ru.msk.sort.quickcheck.SortTestCommand

trait CocktailSort[T] extends SortTestCommand[T] {

  // complexity O(n^2)

  // oriented for List
  override def sort(xs: Seq[T]): Seq[T] = {

    def getOrdFunction (toLast: Boolean): (T,T) => Boolean =
      if (toLast) ord gt else ord lt

    def sortR (toLast: Boolean, ys: List[T], head: List[T], tail: List[T], zs: List[T]): List[T] = ys match {
      case Nil => head ::: tail
      case fst :: Nil =>
        if (toLast) sortR(!toLast, zs, head, fst :: tail, Nil)
        else sortR(!toLast, zs, head ::: List(fst), tail, Nil)
      case fst :: other =>
        if (getOrdFunction(toLast)(fst, other head)) {
          sortR(toLast, fst :: (other tail), head, tail, (other head) :: zs)
        }
        else
          sortR(toLast, other, head, tail, (fst) :: zs)
    }

    sortR(true, xs toList, Nil, Nil, Nil)
  }

}
