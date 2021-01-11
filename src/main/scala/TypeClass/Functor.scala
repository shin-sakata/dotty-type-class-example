package TypeClass

trait Functor[F[_]] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]
}

object Functor {
  def map[A, B, F[_]](f: A => B)(fa: F[A])(using functor: Functor[F]): F[B] =
    functor.map(f)(fa)
}

extension [A, B, F[_]](fa: F[A])(using functor: Functor[F]) {
  def map(f: A => B): F[B] = functor.map(f)(fa)
}
