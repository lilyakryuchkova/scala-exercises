package ru.msk.sort.noncomparative

trait RadixSort[T <: Long] {

  final val radix: Byte = 8

  /*
  * Getting a significant digit on index position
  * */
  def getDigit(value: T, index: Short): Byte = {
//    (value % (math.pow(radix,index)) / (math.pow(radix,index-1))) toByte

    // Eight is the third power of two
    val power = 3 * index
    val prevPower = 3 * (index - 1)
    // Remainder of division value by radix powered index
    val remainder = value & ( (1L << power) -1)

    // Dividing the remainder by radix powered by index-1
    (remainder >> prevPower) toByte
  }

}
