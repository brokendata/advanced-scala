import scala.unchecked
import scala.language.higherKinds
import scalaz.Functor
import scalaz.syntax.functor._

sealed trait Result[+A]
case class Success[A](v: A) extends Result[A]
case class Warning[A](value: A, message: String) extends Result[A]
case class Failure(m: String) extends Result[Nothing]

implicit val resultFunctor = new Functor[Result] {
  def map[A, B](result: Result[A])(func: A => B): Result[B] =
    result match {
      case Success(value) => Success(func(value))
      case Warning(value, message) => Warning(func(value), message)
      case Failure(message) => Failure(message)
    }
}
def success[A](value: A): Result[A] = Success(value)
def warning[A](value: A, message: String): Result[A] = Warning(value, message)
resultFunctor.map(success(1))(_+1)
success(10) map (_ + 2 )