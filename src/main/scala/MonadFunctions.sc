import scalaz._
import Scalaz._

def map2[M[_]: Monad, A,B,C](ma: M[A], mb: M[B])(f: (A,B) => C): M[C] =
  ma.flatMap(a => mb.map(b => f(a,b)))


def sequence[M[_]: Monad, A](lma: List[M[A]]): M[List[A]] =
  lma.foldRight(nil[A].point[M]){
    (z,a) => map2(z,a)(_::_)
  }

def traverse[M[_]: Monad,A,B](la: List[A])(f: A => M[B]): M[List[B]] =
  la.foldRight(nil[B].point[M]){
    case (a,z) => map2(f(a),z)(_::_)
  }

traverse(List(1,2,3,4))(x => some(x + 1))
// def replicateM[A](n: Int, ma: M[A]): M[List[A]]


def replicateM[M[_]: Monad, A](n: Int, ma:M[A]): M[List[A]] = {
  def loop(n: Int, acc: M[List[A]]): M[List[A]] =
  {
    if (n <= 0) acc
    else {loop(n - 1, map2(ma, acc)(_ :: _))}
  }
  loop(n,nil[A].point[M])
}

replicateM(4,some(1))