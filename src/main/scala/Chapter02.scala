object Chapter02 extends App {
  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val rect: Shape = Rectangle(3.0, 4.0)
  val circ: Shape = Circle(1.0)

  def area(shape: Shape): Double =
    shape match {
      case Rectangle(w, h) => w * h
      case Circle(r) => math.Pi * r * r
    }

  println(area(rect))
  println(area(circ))

  type Rectangle2 = (Double, Double)
  type Circle2 = Double
  type Shape2 = Either[Rectangle2, Circle2]

  val rect2: Shape2 = Left((3.0, 4.0))
  val circ2: Shape2 = Right(1.0)

  def area2(shape: Shape2): Double =
    shape match {
      case Left((w, h)) => w * h
      case Right(r) => math.Pi * r * r
    }

  println(area2(rect2))
  println(area2(circ2))

  import shapeless.{HList, ::, HNil}

  val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

  val first = product.head

  println(first)

  val second = product.tail.head

  println(second)

  val rest = product.tail.tail

  println(rest)

  //product.tail.tail.tail.head

  val newProduct: Long :: String :: Int :: Boolean :: HNil = 42L :: product

  import shapeless.Generic

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCreamGen = Generic[IceCream]

  val iceCream = IceCream("Sundae", 1, false)

  val repr = iceCreamGen.to(iceCream)

  val iceCream2 = iceCreamGen.from(repr)

  case class Employee(name: String, number: Int, manager: Boolean)

  val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

  val tupleGen = Generic[(String, Int, Boolean)]

  tupleGen.to(("Hello", 123, true))

  tupleGen.from("Hello" :: 123 :: true :: HNil)

  case class BigData(
    a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int,
    k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int,
    u: Int, v: Int, w: Int)

  Generic[BigData].from(Generic[BigData].to(BigData(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)))

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

  case class Red()
  case class Amber()
  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  val red: Light = Inl(Red())

  val green: Light = Inr(Inr(Inl(Green())))

  val gen = Generic[Shape]

  println(gen.to(Rectangle(3.0, 4.0)))
  println(gen.to(Circle(1.0)))

}
