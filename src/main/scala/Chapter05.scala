import shapeless._
import shapeless.labelled.{field, KeyTag, FieldType}
import shapeless.syntax.singleton._
import shapeless.ops.hlist.{IsHCons, Last}

import scala.reflect.runtime.universe.TypeTag

object Chapter05 extends App {

  def showType[A](value: A)(implicit ct: TypeTag[A]): Unit = println(ct.tpe.toString)

  showType(1.narrow)

  val number = 42

  trait Cherries

  val numCherries = number.asInstanceOf[Int with Cherries]

  showType(numCherries)

  val someNumber = 123

  val numCherries2 = "numCherries" ->> someNumber

  showType(numCherries2)

  showType(field[Cherries](123))

  // type FieldType[K, V] = V with KeyTag[K, V]

  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K = witness.value

  println(getFieldName(numCherries2))

  def getFieldValue[K, V](value: FieldType[K, V]): V = value

  println(getFieldValue(numCherries2))

  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil

  showType(garfield)


  sealed trait JsonValue
  case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
  case class JsonArray(items: List[JsonValue]) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }

  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  }

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] =
    (value: A) => func(value)

  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => JsonString(str))

  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(num => JsonNumber(num))

  implicit val intEncoder: JsonEncoder[Int] =
    createEncoder(num => JsonNumber(num))

  implicit val booleanEncoder: JsonEncoder[Boolean] =
    createEncoder(bool => JsonBoolean(bool))

  implicit def listEncoder[A]
  (implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(list => JsonArray(list.map(enc.encode)))

  implicit def optionEncoder[A]
  (implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCream = IceCream("Sundae", 1, false)

  val iceCreamJson: JsonValue =
    JsonObject(List(
      "name" -> JsonString("Sundae"),
      "numCherries" -> JsonNumber(1),
      "inCone" -> JsonBoolean(false)
    ))

  val gen = LabelledGeneric[IceCream].to(iceCream)

  showType(gen)

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): JsonObject
  }

  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
    (value: A) => fn(value)

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }


  implicit def genericObjectEncoder[A, H <: Product](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }

  showType(JsonEncoder[IceCream].encode(iceCream))
  println(JsonEncoder[IceCream].encode(iceCream))

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  showType(LabelledGeneric[Shape].to(Circle(1.0)))

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
    createObjectEncoder(_ => throw new Exception("Inconceivable!"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val typeName = witness.value.name
    createObjectEncoder {
      case Inl(h) =>
        JsonObject(List(typeName -> hEncoder.value.encode(h)))

      case Inr(t) =>
        tEncoder.encode(t)
    }
  }

  val shape: Shape = Circle(1.0)

  showType(JsonEncoder[Shape].encode(shape))
  println(JsonEncoder[Shape].encode(shape))

}
