package ru.msk.sort.comparative.merge

import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait SimpleMergeSort[T] extends  SortTestCommand[T]{

  // oriented to List
  override def sort(xs: Seq[T]): Seq[T] = {

    @tailrec
    def merge (ts: Seq[T], ps: Seq[T], acc: Seq[T] = Seq[T]()): Seq[T] = (ts, ps) match {
      case (Seq(), ps) => acc ++ ps
      case (ts, Seq()) => acc ++ ts
      case (Seq(t, ts1 @ _*), Seq(p, ps1 @ _*)) =>
        if (ord lt (t, p))
          merge(ts1, ps, acc :+ t)
        else
          merge(ts, ps1, acc :+ p)
    }
/*
    def msort(pair: (Seq[T], Seq[T]), acc: Seq[(Seq[T], Seq[T])] = Seq()): Seq[T] = {
      val n = (ys size) / 2
      if (n == 0) ys
      else {

        val (fst, snd) = ys splitAt n
        msort(ys splitAt n)
        msort(, (fst, snd) +: acc)
        merge(msort(fst), msort(snd))
      }

    }

    msort(xs)
*/
    Seq()
  }

}
