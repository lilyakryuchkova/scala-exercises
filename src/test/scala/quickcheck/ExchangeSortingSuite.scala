package quickcheck

import org.scalacheck.Gen
import org.scalacheck.Gen.posNum
import ru.msk.sort.comparative.exchange._
import ru.msk.sort.comparative.exchange.quicksort.{Hoare, MedianOfThree, Middle, ThreeWay}


//@RunWith(classOf[JUnitRunner])
class ExchangeSortingSuite extends QuickCheckSuite {

  test("Simple int sort list") {
    val t = new QuickCheckCommand[Seq[Int]](new SimpleSort[Int] {}) {
      lazy val generator = Gen.listOfN[Int](100, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Bubble int sort list") {
    val t = new QuickCheckCommand[Seq[Int]](new BubbleSort[Int] {
      override val debug = false
    }) {
      lazy val generator = Gen.listOfN[Int](100, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Shaker int sort list") {
    val t = new QuickCheckCommand[Seq[Int]](new CocktailSort[Int] {}) {
      lazy val generator = Gen.listOfN[Int](3000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Gnome int sort vector") {
    val t = new QuickCheckCommand[Seq[Int]](new GnomeSort[Int] {
      override val debug = false
    }) {
      lazy val generator = Gen.listOfN[Int](1000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Comb int sort vector") {
    val t = new QuickCheckCommand[Seq[Int]](new CombSort[Int] {
      override val debug = false
    }) {
      lazy val generator = Gen.listOfN[Int](5000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Quicksort int sort vector with Hoare partion and MedianOfThree pivot") {
    val t = new QuickCheckCommand[Seq[Int]](new QuickSort[Int] {
      override val debug = false
      override val partition = new Hoare[Int](new MedianOfThree[Int])
    }) {
      lazy val generator = Gen.listOfN[Int](1000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Quicksort int sort vector with Three way partion and MedianOfThree pivot") {
    val t = new QuickCheckCommand[Seq[Int]](new QuickSort[Int] {
      override val debug = false
      override val partition = new ThreeWay[Int](new MedianOfThree[Int])
    }) {
      lazy val generator = Gen.listOfN[Int](5000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Quicksort int sort vector with Hoare partion and Middle pivot") {
    val t = new QuickCheckCommand[Seq[Int]](new QuickSort[Int] {
      override val debug = false
      override val partition = new Hoare[Int](new Middle[Int])
    }) {
      lazy val generator = Gen.listOfN[Int](5000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Sorting with determed set") {
    val set = List(2,
    3,
    2,
    1,
    1)

    val command = (new QuickSort[Int] {
      override val debug = true
      override val partition = new ThreeWay[Int](new MedianOfThree[Int])
    })

    assert(command checkResult(set, command sort (set)))
  }
}
