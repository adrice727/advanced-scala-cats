import cats.syntax.show._


trait Printable[A] {
  def format(input: A): String
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    def format(input: String): String = input
  }
  implicit val intPrintable = new Printable[Int] {
    def format(input: Int): String = input.toString
  }
  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat): String = s"${cat.name} is a ${cat.age} year-old ${cat.color} cat"
  }
}

object PrintableSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)
  def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
}

final case class Cat(
  name: String,
  age: Int,
  color: String
)

implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat")