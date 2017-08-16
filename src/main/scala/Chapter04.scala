import shapeless._
import shapeless.ops.hlist.{IsHCons, Last}

import scala.reflect.runtime.universe.TypeTag

object Chapter04 extends App {

  def getRepr[A](value: A)(implicit gen: Generic[A]) =
    gen.to(value)

  def showType[A](value: A)(implicit ct: TypeTag[A]): Unit = println(ct.tpe.toString)

  case class Vec(x: Int, y: Int)

  case class Rect(origin: Vec, size: Vec)

  showType(getRepr(Vec(0, 0)))
  showType(getRepr(Rect(Vec(0, 0), Vec(4, 4))))

  /*
   * Useless in this case
   *
  trait Generic2[A, Repr]

  def getRepr2[A, R](value: A)(implicit generic: Generic2[A, R]): R =
    ???
  */

  val last1 = Last[String :: Int :: HNil]
  val last2 = Last[Int :: String :: HNil]

  println(last1("foo" :: 123 :: HNil))
  println(last2(321 :: "bar" :: HNil))

  // Last[HNil]

  //last1(321 :: "bar" :: HNil)


  trait Second[L <: HList] {
    type Out

    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] {type Out = O}

    def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] =
      inst

  }

  showType(implicitly[Last[String :: Int :: HNil]])
  showType(Last[String :: Int :: HNil])
  showType(the[Last[String :: Int :: HNil]])

  implicit def hlistSecond[A, B, Rest <: HList]: Second.Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest] {
      type Out = B

      def apply(value: A :: B :: Rest): B =
        value.tail.head
    }

  implicit def hlist2Second[A, B, Rest <: HList]: Second[A :: B :: Rest] =
    new Second[A :: B :: Rest] {
      type Out = B

      def apply(value: A :: B :: Rest): B =
        value.tail.head
    }

  val second1 = Second[String :: Boolean :: Int :: HNil]
  val second2 = Second[String :: Int :: Boolean :: HNil]

  //showType(hlistSecond[String, Int, String :: Boolean :: Int :: HNil])

  //Second[String :: HNil]

  println(second1("foo" :: true :: 123 :: HNil))
  println(second2("bar" :: 321 :: false :: HNil))
  //second1("baz" :: HNil)

  showType(hlistSecond[String, Boolean, String :: Boolean :: HNil])
  showType(hlist2Second[String, Boolean, String :: Boolean :: HNil])

  val test: Second[String :: Boolean :: HNil] = hlistSecond[String, Boolean, HNil]

  println(Second[String :: Boolean :: Int :: HNil])

  def lastField[A, Repr <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  println(lastField(Rect(Vec(1, 2), Vec(3, 4))))

  def getWrappedValue[A, Repr <: HList, Head](in: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(in).head

  case class Wrapper(value: Int)

  println(getWrappedValue(Wrapper(42)))

  def getWrappedValue2[A, Repr <: HList, Tail <: HList, Second](in: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, _, Tail],
    isHCons2: IsHCons.Aux[Tail, Second, HNil]
  ): Second = gen.to(in).tail.head

  case class Wrapper2(i: Int, d: Double)

  println(getWrappedValue2(Wrapper2(1, 1.0)))

  //case class Wrapper3(i: Int, d: Double, s: String)
  //println(getWrappedValue2(Wrapper3(1, 1.0, "1")))

}
