package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckCommand[T](command: TestTransformCommand[T]) extends Properties("QuickCheckCommand") {

  val generator: Gen[T]

  implicit lazy val arbIntList: Arbitrary[T] = Arbitrary(generator)

  property("check command") = forAll { (value: T) =>
    //println("..1")
    val result = command execute value
    //println("..2")
    val t = command checkResult (value, result)

    if (command debug) {
      println("_______________________")
    }
    t
  }


}
