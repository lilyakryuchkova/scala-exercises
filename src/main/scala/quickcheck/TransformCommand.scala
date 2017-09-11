package quickcheck

trait TransformCommand[T] {

  def execute (value: T): T

}
