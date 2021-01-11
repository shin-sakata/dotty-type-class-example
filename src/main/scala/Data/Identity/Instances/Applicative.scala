package Data.Identity.Instances

import TypeClass.{Applicative, Functor}
import Data.Identity._

given (using functor: Functor[Identity]) as Applicative[Identity] {
  def map[A, B](f: A => B)(fa: Identity[A]): Identity[B] = functor.map(f)(fa)
  def pure[A](a: A): Identity[A] = Identity(a)
  def ap[A, B](ff: Identity[A => B])(fa: Identity[A]): Identity[B] = 
    ff match {
      case Identity(f) => functor.map(f)(fa)
    }
}
