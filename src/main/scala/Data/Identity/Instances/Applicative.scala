package Data.Identity.Instances

import TypeClass.{Applicative, Functor}
import Data.Identity._

given (using functor: Functor[Identity]): Applicative[Identity] with
  export functor._

  def pure[A](a: A): Identity[A] = Identity(a)
  def ap[A, B](ff: Identity[A => B])(fa: Identity[A]): Identity[B] = 
    ff match {
      case Identity(f) => functor.map(f)(fa)
    }
