package quickcheck

import org.scalacheck.Gen
import org.scalacheck.Gen.posNum
import ru.msk.sort.comparative.insertion.shellgaps._
import ru.msk.sort.comparative.insertion.{ShellSort, SimpleInsertionSort}


class InsertionSortingSuite extends QuickCheckSuite {

  test("Simple insertion int sort list") {
    val t = new QuickCheckCommand[Seq[Int]](new SimpleInsertionSort[Int] {
      override val debug = false
    }) {
      lazy val generator = Gen.listOfN[Int](300, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Shell insertion int sort list with gaps (N / (2 ^ k))") {
    val t = new QuickCheckCommand[Seq[Int]](new ShellSort[Int] {
      override val debug = false
      override val gapsGenerator: Gap = ShellGap
    }) {
      lazy val generator = Gen.listOfN[Int](5000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Shell insertion int sort list with gaps 4 ^ k + 3 * (2 ^ (k-1)) + 1") {
    val t = new QuickCheckCommand[Seq[Int]](new ShellSort[Int] {
      override val debug = false
      override val gapsGenerator: Gap = Sedgewick
    }) {
      lazy val generator = Gen.listOfN[Int](5000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Shell insertion int sort list with gaps 5, 3, 1") {
    val t = new QuickCheckCommand[Seq[Int]](new ShellSort[Int] {
      override val debug = false
      override val gapsGenerator: Gap = Test
    }) {
      lazy val generator = Gen.listOfN[Int](12, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Shell insertion int sort list with gaps Ciura") {
    val t = new QuickCheckCommand[Seq[Int]](new ShellSort[Int] {
      override val debug = false
      override val gapsGenerator: Gap = Ciura
    }) {
      lazy val generator = Gen.listOfN[Int](5000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Sorting with determed set") {
    val set = List(1,
    1,
    2,
    2,
    2,
    2,
    1,
    1,
    3,
    3)

    val command = (new ShellSort[Int] {
      override val debug = true
      override val gapsGenerator: Gap = Sedgewick
    })

    assert(command checkResult(set, command sort (set)))
  }
}
