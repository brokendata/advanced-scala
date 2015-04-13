import scala.language.higherKinds

sealed trait Functor[F[_]]{
  def mapz[A,B](fa: F[A])(f: A => B): F[B]
}
object Functor{
  def apply[F[_]: Functor] = implicitly[Functor[F]]

  implicit def listFunctor = new Functor[List]{
    def mapz[A,B](fa: List[A])(f: A => B): List[B] = {
      def loop(ls: List[A], acc: List[B]): List[B] = ls match {
        case Nil => acc
        case x :: xs => loop(xs, f(x) :: acc)
      }
      loop(fa,Nil).reverse
    }
  }
}
Functor[List].mapz(List(1,2,3))(_.toString)

object FunctorSyntax{
  implicit class FSyntax[F[_]: Functor,A](v: F[A]){
   def mapz[B](f: A =>B): F[B] = implicitly[Functor[F]].mapz(v)(f)
  }
}

import Functor._
import FunctorSyntax._

Functor[List].mapz(List(1,2,3,4))(x => x +1)
List(1,2,3).mapz(_ +1)