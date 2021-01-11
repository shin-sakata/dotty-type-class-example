import org.scalatest.flatspec.AnyFlatSpec
import Data.Identity.Identity

class IdentitySpec extends AnyFlatSpec {
  {
    import Data.Identity.Instances.given_Functor_Identity
    import TypeClass.Functor.map
    import TypeClass._

    "map関数" should "中身の値に関数を適用できる" in {
      val identity = Identity(1)
      def f = (x: Int) => x * 2

      assert(map(f)(identity) === Identity(2))
      assert(map(f)(map(f)(identity)) === Identity(4))
      assert(map(f compose f)(identity) === map(f)(map(f)(identity)))
    }

    "map関数" should "Functor即を満たしている" in {
      val identity1 = Identity(1)
      def id[T] = (x: T) => x

      assert(map(id)(identity1) === identity1)
    }

    "mapメソッド" should "中身の値に関数を適用できる" in {
      val identity = Identity(1)
      def f = (x: Int) => x * 2

      assert(identity.map(f) === Identity(2))

      assert(identity.map(f).map(f) === Identity(4))
      assert(identity.map(f compose f) === identity.map(f).map(f))
    }

    "mapメソッド" should "Functor即を満たしている" in {
      val identity1 = Identity(1)
      def id[T] = (x: T) => x

      assert(identity1.map(id) === identity1)
    }
  }
  
  
  {
    import Data.Identity.Instances.{given_Functor_Identity, given_Applicative_Identity}
    import TypeClass.Applicative.{ap, pure}
    import TypeClass._
    
    "pure関数" should "持ち上げ" in {
      val identity1 = pure(1)
      
      assert(identity1 === Identity(1))
    }
    
    "ap関数" should "複数の引数を取れる" in {
      val identityPlus = pure((x: Int) => (y: Int) => x + y)
      val identity1 = pure(1)
      val identity2 = pure(2)
      
      assert(ap(ap(identityPlus)(identity1))(identity2) === Identity(3))
    }
    
    "apメソッド" should "複数の引数を取れる" in {
      val identityPlus = pure((x: Int) => (y: Int) => x + y)
      val identity1 = pure(1)
      val identity2 = pure(2)
      
      assert(identityPlus <*> identity1 <*> identity2 === Identity(3))
    }
  }

  {
    import Data.Identity.Instances.{given_Functor_Identity, given_Applicative_Identity, given_Monad_Identity}
    import TypeClass.Monad.flatMap
    import TypeClass.Applicative.pure
    import TypeClass._

    "flatMap関数" should "文脈が持ち上がる計算をflatに保つ" in {
      val liftIncrement = (x: Int) => pure(x + 1)
      val identity1: Identity[Int] = pure(1)

      assert(flatMap(liftIncrement)(identity1) === Identity(2))
    }
    
    "flatMapメソッド" should "文脈が持ち上がる計算をflatに保つ" in {
      val liftIncrement = (x: Int) => pure(x + 1)
      val identity1: Identity[Int] = pure(1)

      assert((identity1 >>= liftIncrement >>= liftIncrement) === Identity(3))
    }
  }
  
  {
    import Data.Identity.Instances.{given_Functor_Identity, given_Applicative_Identity, given_Monad_Identity}
    import TypeClass.Applicative.pure
    import TypeClass._
    
    "Identityモナド" should "for-yeild" in {
      val result = for {
        a <- pure(1)
        b <- pure(2)
        c = a + b
      } yield (c + a + b)
      
      assert(result === Identity(6))
    }
  }
}
