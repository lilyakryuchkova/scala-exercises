package ru.msk.sort.comparative.exchange

import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait BubbleSort [T] extends SortTestCommand[T] {

  @tailrec
  private def h(unseened: Seq[T], seened: Seq[T] = Seq[T](), sortedTail: Seq[T] = Seq[T]()): Seq[T] = unseened match {
    case Seq() => sortedTail
    case Seq(x) => h(seened, Seq[T](), x +: sortedTail)
    case Seq(x, tail @ _*) =>
      if (ord gt (tail head, x))
        h(tail, seened :+ x, sortedTail)
      else
        h(x +: (tail tail), seened :+ (tail head), sortedTail)
  }

  override def sort(xs: Seq[T]): Seq[T] = {
    h(xs)
  }

}
