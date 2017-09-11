package quickcheck

import org.scalacheck.Gen
import org.scalacheck.Gen.posNum
import ru.msk.sort.comparative.selection.HeapSort

class SelectionSortingSuite extends QuickCheckSuite {

  test("Heapsort int sort list") {
    val t = new QuickCheckCommand[Seq[Int]](new HeapSort[Int] {
      override val debug = false
    }) {
      lazy val generator = Gen.listOfN[Int](5000, posNum[Int])
    }

    assert(checkCommand(t))
  }

  test("Sorting with determed set") {
    val set = List(3,
    6,
    6,
    1,
    4)

    val command = (new HeapSort[Int] {
      override val debug = true
    })

    assert(command checkResult(set, command sort (set)))
  }

}
