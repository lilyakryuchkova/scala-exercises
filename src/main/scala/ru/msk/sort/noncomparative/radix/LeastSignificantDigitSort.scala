package ru.msk.sort.noncomparative.radix

import ru.msk.sort.noncomparative.RadixSort
import ru.msk.sort.quickcheck.SortTestCommand

import scala.annotation.tailrec

trait LeastSignificantDigitSort[T <: Long] extends SortTestCommand[T] with RadixSort[T] {

  // Average case   O(n * w)

  type Input = Seq[T]
  // type for baskets
  type Basket = List[T]

  @tailrec
  private def unloadBaskets (baskets: Map[Byte, Basket],
                                 basketsCount: Int,
                                 acc: Input = Seq(), basketIndex: Byte = 0): Input = {

    // if we are collect all elements
    if (basketsCount == 0) acc
    else {

      baskets get basketIndex match {
        case None =>
          unloadBaskets(baskets, basketsCount, acc, (basketIndex + 1) toByte)
        case Some(basket) => {
          @tailrec
          def unload(b: Basket, arr: Input = acc): Input = b match {
            case Nil => arr
            case element :: tailIndices => {
              unload(tailIndices, arr :+ element)
            }
          }

          val partedAcc = unload(basket, acc)
          unloadBaskets(baskets, basketsCount - 1, partedAcc, (basketIndex + 1) toByte)
        }
      }
    }
  }

  // Loading baskets by significant digit on current position in numbers
  private def loadingBaskets(ys: Input, digit: Short = 1, baskets: Map[Byte, Basket] = Map[Byte, Basket]()): Input = ys match {

    // reach the end of list
    case Seq() =>
      val partied = unloadBaskets(baskets, (baskets keySet) size)

      // if the step exposed only one "0" digit - we have reached the end of algorithm
      if (((baskets keySet) size) == 1) {

        baskets get 0 match {
          case None =>
            loadingBaskets(partied, (digit + 1) toShort)
          case _ =>
            partied
        }

        // if the step exposed more than one basket - we should continue
      } else {
        loadingBaskets(partied, (digit + 1) toShort)
      }

    // walk through the vector
    case Seq(element, others @ _*) => {
      // Take the least significant digit
      val basketIndex = getDigit(element, digit)

      // Group the keys based on that digit, but otherwise keep the original order of keys
      val basket = baskets get basketIndex match {
        case Some(content) => content ::: List(element)
        case None => List(element)
      }

      loadingBaskets(others, digit, baskets + (basketIndex -> basket))
    }
  }

  override def sort(xs: Input): Input = {
    loadingBaskets(xs)
  }

}
