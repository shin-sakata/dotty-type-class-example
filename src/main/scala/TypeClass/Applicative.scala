package TypeClass

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](f: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  def pure[A, F[_]](a: A)(using applicative: Applicative[F]): F[A] =
    applicative.pure(a)

  def ap[A, B, F[_]](f: F[A => B])(fa: F[A])(using applicative: Applicative[F]): F[B] = 
    applicative.ap(f)(fa)
}

extension [A, B, F[_]](ff: F[A => B])(using applicative: Applicative[F]) {
  def ap(fa: F[A]) = applicative.ap(ff)(fa)
  def <*>(fa: F[A]) = applicative.ap(ff)(fa)
}