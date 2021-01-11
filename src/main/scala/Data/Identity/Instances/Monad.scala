package Data.Identity.Instances

import TypeClass.{Applicative, Monad}
import Data.Identity._

given (using applicative: Applicative[Identity]) as Monad[Identity] {
  def map[A, B](f: A => B)(fa: Identity[A]): Identity[B] =
    applicative.map(f)(fa)
  def pure[A](a: A): Identity[A] =
    applicative.pure(a)
  def ap[A, B](ff: Identity[A => B])(fa: Identity[A]): Identity[B] = 
    applicative.ap(ff)(fa)

  def flatMap[A, B](f: A => Identity[B])(fa: Identity[A]): Identity[B] = 
    fa match {
      case Identity(a) => f(a)
    }
}
