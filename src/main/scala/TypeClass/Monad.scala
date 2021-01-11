package TypeClass

trait Monad[M[_]] extends Applicative[M] {
  def flatMap[A, B](f: A => M[B])(fa: M[A]): M[B]
}

object Monad {
  def flatMap[A, B, M[_]](f: A => M[B])(ma: M[A])(using monad: Monad[M]): M[B] =
    monad.flatMap(f)(ma)
}

extension [A, B, M[_]](fa: M[A])(using monad: Monad[M]) {
  def flatMap(f: A => M[B]): M[B] = monad.flatMap(f)(fa)
  def >>=(f: A => M[B]): M[B] = monad.flatMap(f)(fa)
}
