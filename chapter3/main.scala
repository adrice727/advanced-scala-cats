import cats.Functor
import cats.instances.function._
import cats.syntax.functor._


sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]


implicit val treeFunctor = new Functor[Tree] {
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    case Leaf(a) => Leaf(f(a))
  }
}
def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)