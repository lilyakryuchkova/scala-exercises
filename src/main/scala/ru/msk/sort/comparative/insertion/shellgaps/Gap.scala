package ru.msk.sort.comparative.insertion.shellgaps

import scala.annotation.tailrec

trait Gap {

  def gaps (n: Int): Seq[Int]

}

object ShellGap  extends Gap {

  // N / (2 ^ k)
  // worst case complexity  Teta ( n * n)

  def gaps (n: Int): Seq[Int] = {

    @tailrec
    def degrees (n: Int, k: Int): Int = {
      val t = n >> 1
      if (t > 0) degrees(t, k + 1)
      else k
    }

    @tailrec
    def helper(k: Int, acc: Seq[Int]): Seq[Int] = {
      if (k > 0) {
        val d: Int = (n / Math.pow(2, k)) toInt

        helper(k - 1, d +: acc)
      } else acc
    }

    helper(degrees(n, 0), Nil)
  }

}

object Sedgewick extends Gap {

  // 4 ^ k + 3 * (2 ^ (k-1)) + 1
  // worst-case complexity O(n ^ 3/4)

  private def f (k: Int): Int = k match {
    case 0 => 1
    case _ => (Math.pow(4, k) + 3 * Math.pow(2, k-1) + 1) toInt
  }

  def gaps (n: Int): Seq[Int] = {

    @tailrec
    def helper(k: Int, acc: Seq[Int]): Seq[Int] = {
      val d: Int = f(k)
      if (d < n)
        helper(k + 1, d +: acc )
      else
        acc
    }

    helper(0, Seq())
  }
}

object Ciura extends Gap {

  // experimental 1, 4, 10, 23, 57, 132, 301, 701

  def gaps(n: Int): Seq[Int] =
    Seq(1, 4, 10, 23, 57, 132, 301, 701) reverse
}

object Test extends Gap {

  // test 1, 3 , 5
  def gaps(n: Int): Seq[Int] =
    Seq(1, 3, 5) reverse

}