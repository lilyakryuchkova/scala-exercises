package quickcheck

import org.scalacheck.Gen
import org.scalacheck.Gen.posNum
import ru.msk.sort.noncomparative.radix.LeastSignificantDigitSort

class NonComparationSortingSuite extends QuickCheckSuite {

  test("LST with long List") {
    val t = new QuickCheckCommand[Seq[Long]](new LeastSignificantDigitSort[Long] {
      override val debug = false
    }) {
      lazy val generator = Gen.listOfN[Long](1000, posNum[Long])
    }

    assert(checkCommand(t))
  }


  test("Sorting with determed set") {
    val set: Seq[Long] = List(2L,
    31231212322222L,
    112334231321323L,
    1231311123123L,
    3L)

    val command = (new LeastSignificantDigitSort[Long] {
      override val debug = true
    })

    assert(command checkResult(set, command sort (set)))
  }
}
