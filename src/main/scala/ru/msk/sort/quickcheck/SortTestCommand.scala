package ru.msk.sort.quickcheck

import quickcheck.TestTransformCommand

import scala.annotation.tailrec

abstract class SortTestCommand[T] (protected implicit val ord: Ordering[T]) extends TestTransformCommand[Seq[T]] {

  @tailrec
  private def isSorted(xs: Seq[T]): Boolean = xs match {
    case Seq()  => true
    case Seq(x) => true
    case Seq(x, ys @ _*) => {
      if (ord gt(x, ys head)) false
      else isSorted (ys)
    }
  }

  final override def checkResult(oldValue: Seq[T], newValue: Seq[T]): Boolean = {
    val t = isSorted(newValue)
    if (debug) {
      println(s"isSorted=$t {lengths the same}=${oldValue.size == newValue.size}")
    }
    t && (oldValue.size == newValue.size)
  }

  def sort(xs: Seq[T]): Seq[T]

  final override def execute(value: Seq[T]): Seq[T] = {
    if (debug) {
      value foreach println
    }
    val t = sort (value)
    if (debug) {
      println("sorting result")
      t foreach println
    }
    t
  }

}
