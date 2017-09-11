package ru.msk.sort.comparative.exchange.quicksort

import scala.annotation.tailrec

trait Pivot[T] {

  def get(xs: Seq[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): Int
}


final class Middle[T] extends Pivot[T] {

  def get(xs: Seq[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): Int = {
    lo + (hi-lo) / 2
  }
}

/*
* Median of first, middle and last elements
* */
final class MedianOfThree[T] extends Pivot[T]{

  def get(xs: Seq[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): Int = {

    val indexes: List[Int] = List(lo, lo + (hi-lo) / 2, hi)

    @tailrec
    def insert(index: Int, ts: List[Int], acc: List[Int] = List()): List[Int] = ts match {
      case Nil => acc ::: List(index)
      case t :: tail =>
        if (ord gt(xs(index), xs(t)))
          insert(index, tail, acc ::: List(t))
        else
          acc ::: index :: ts
    }

    /*
    * Sorting with min*/
    @tailrec
    def sort (ys: List[Int], sorted: List[Int] = List()): List[Int] = ys match {
      case Nil => sorted
      case index :: others => {
        sort(others, insert(index, sorted))
      }
    }

    def getMinIndex(ys: List[Int], minIndex: Int): Int = ys match {
      case Nil => minIndex
      case index :: others => {
        if (ord gt(xs(minIndex), xs(index)))
          getMinIndex(others, index)
        else
          getMinIndex(others, minIndex)
      }
    }

    val minIndex = getMinIndex(indexes, indexes head)
    val others = indexes filter (_ != minIndex)
    others match {
      case firstIndex :: Nil => firstIndex
      case firstIndex :: secondIndex :: Nil =>
        if (ord gt(xs(firstIndex), xs(secondIndex)))
          secondIndex
        else
          firstIndex
    }

    // median is a middle of sorting list
//    val sorted = sort(indexes)
//    sorted(1)

  }
}
