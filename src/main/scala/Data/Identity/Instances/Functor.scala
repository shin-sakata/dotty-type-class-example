package Data.Identity.Instances

import TypeClass.Functor
import Data.Identity._

given Functor[Identity] with
  def map[A, B](f: A => B)(fa: Identity[A]): Identity[B] =
    fa match {
      case Identity(value) => Identity(f(value))
    }
