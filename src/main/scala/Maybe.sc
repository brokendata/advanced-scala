import scalaz.Monad
import scalaz.syntax.monad._
import scalaz.std.AllInstances._
import scalaz.Functor
import scalaz.Monoid
import scalaz.syntax.monoid._


sealed trait Maybe[+A] {
  def map[B](f: A => B): Maybe[B] = this match {
    case Non => Non
    case Just(a) => Just(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Non => default
    case Just(a) => a
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = map(f) getOrElse Non

}
final case class Just[+A](v: A) extends Maybe[A]
final case object Non extends Maybe[Nothing]

object Maybe{
  implicit val maybeMonad = new Monad[Maybe]{
    def bind[A,B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] = fa flatMap f
    def point[A](a: => A) = Just(a)
  }

}

Functor[Maybe].map(Just(10))(x => x +1)
Monad[Maybe].point(1)


def foldMapM[A, M[_] : Monad, B: Monoid](iter: Iterable[A])(f: A => M[B]): M[B] =
  iter.foldLeft(mzero[B].point[M]){(acc, z) =>
    for {
      v1 <- acc
      v2 <- f(z)
    } yield v1 |+| v2
  }


