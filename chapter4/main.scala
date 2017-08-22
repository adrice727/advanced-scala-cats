import scala.language.higherKinds
import scala.annotation.tailrec
import cats.Monad

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A,B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))

}


import cats.Id

trait Id extends Monad[Id[A]]{
  def pure[A](value: A): Id[A] = value

def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)
}


import cats.Eval


def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = {
  as match {
    case head :: tail => Eval.delay(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil => Eval.now(acc)
  }
}


sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)


val treeMonad = new Monad[Tree] {
  def pure[A](a: A) = Leaf(a)
  def flatMap[A, B](t: Tree[A])(f: A => Tree[B]): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
  }

  @tailrec
  def tailRecM[A, B](arg: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(arg) match {
    case Leaf(Left(a)) => Leaf(Left(a))
    case Leaf(Right(a)) => tailRecM(a)(f)
    case Branch(l, r) => Branch(
      flatMap(l) match {
        case Left(a) => pure(a)
        case Right(a) => tailRecM(a)(f)
      },
      flatMap(r) match {
        case Left(a) => pure(a)
        case Right(a) => tailRecM(a)(f)
      },
    )
  }

}
