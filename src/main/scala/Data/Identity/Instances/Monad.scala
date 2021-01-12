package Data.Identity.Instances

import TypeClass.{Applicative, Monad}
import Data.Identity._

given (using applicative: Applicative[Identity]): Monad[Identity] with
  export applicative._

  def join[A](mma: Identity[Identity[A]]): Identity[A] = {
    mma match {
      case Identity(ma) => ma
    }
  }
