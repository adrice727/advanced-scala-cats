
trait Printable[A] {
  def format(input: A): String
}

object PrintableInstances = {
  implicit val stringPrintable = new Printable[String] {
    def format(input: String): String = input
  }
  implicit val intPrintable = new Printable[Int] {
    def format(input: Int): String = input.toString
  }
}

object Printable {
  implicit def format[A](input: A, implicit p: Printable[A]): String = p.format(input)
  implicit def print[A](input: A, implicit p: Printable[A]): Unit = println(format(input))
}

