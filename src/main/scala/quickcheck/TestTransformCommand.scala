package quickcheck

abstract class TestTransformCommand[T](val debug: Boolean = false) extends TransformCommand[T] {

  def checkResult (oldValue: T, newValue: T): Boolean

}
