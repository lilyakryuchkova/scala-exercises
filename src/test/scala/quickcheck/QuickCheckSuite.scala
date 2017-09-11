package quickcheck

import org.junit.runner.RunWith
import org.scalacheck.Properties
import org.scalacheck.Test.{Parameters, checkProperties}
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers


@RunWith(classOf[JUnitRunner])
abstract class QuickCheckSuite extends FunSuite with Checkers {

  def checkCommand(properties: Properties): Boolean = {
    val testParams = Parameters.default
        .withMinSuccessfulTests(100)
        //.withMinSize(1)

    checkProperties(testParams, properties).foldLeft (true) { case (a, result) =>
      val (name, res) = result
      println(s"$name: ${res.passed}")
      a && res.passed
    }
  }
}
