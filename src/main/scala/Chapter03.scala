object Chapter03 extends App {
  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  case class Employee(name: String, number: Int, manager: Boolean)

  /*
  implicit val employeeEncoder = new CsvEncoder[Employee] {
    def encode(value: Employee): List[String] =
      List(value.name, value.number.toString, value.manager.toString)
  }
  */

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  val employees = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  //println(writeCsv(employees))

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  /*
  implicit val iceCreamEncoder: CsvEncoder[IceCream] =
    new CsvEncoder[IceCream] {
      def encode(i: IceCream): List[String] =
        List(
          i.name,
          i.numCherries.toString,
          if (i.inCone) "yes" else "no"
        )
    }
  */

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )


  //println(writeCsv(iceCreams))

  implicit def pairEncoder[A, B](implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]) =
    new CsvEncoder[(A, B)] {
      def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }

  //println(writeCsv(employees zip iceCreams))

  object CsvEncoder {
    // "Summoner" method
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
      enc

    // "Constructor" method
    def instance[A](func: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        def encode(value: A): List[String] =
          func(value)
      }

    // Globally visible type class instances
  }

  //println(CsvEncoder[IceCream])

  //println(implicitly[CsvEncoder[IceCream]])

  import shapeless.the

  //println(the[CsvEncoder[IceCream]])

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] = func(value)
    }

  implicit val stringEncoder: CsvEncoder[String] =
    createEncoder(str => List(str))

  implicit val intEncoder: CsvEncoder[Int] =
    createEncoder(num => List(num.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    createEncoder(bool => List(if (bool) "yes" else "no"))

  import shapeless.{HList, ::, HNil}

  implicit val hnilEncoder: CsvEncoder[HNil] =
    createEncoder(hnil => Nil)

  /*
  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: CsvEncoder[H],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }
  */

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  println(reprEncoder.encode("abc" :: 123 :: true :: HNil))

  import shapeless.Generic

  /*
  implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
    val gen = Generic[IceCream]
    val enc = CsvEncoder[gen.Repr]
    createEncoder(iceCream => enc.encode(gen.to(iceCream)))
  }
  */

  /*
  implicit def genericEncoder[A, R](
    implicit
    //gen: Generic[A] {type Repr = R},
    gen: Generic.Aux[A, R],
    enc: CsvEncoder[R]
  ): CsvEncoder[A] = {
    createEncoder(a => enc.encode(gen.to(a)))
  }
  */

  println(writeCsv(iceCreams))

  println(implicitly[Generic[IceCream]])

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  println(implicitly[Generic[Shape]])

  println(implicitly[Generic[Shape]])

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr, Lazy}

  implicit val cnilEncoder: CsvEncoder[CNil] =
    createEncoder(cnil => throw new Exception("Inconceivable!"))

  /*
  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
    hEncoder: CsvEncoder[H],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }
  */

  val shapes: List[Shape] = List(
    Rectangle(3.0, 4.0),
    Circle(1.0)
  )

  implicit val doubleEncoder: CsvEncoder[Double] =
    createEncoder(d => List(d.toString))

  println(writeCsv(shapes))

  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] = createEncoder {
    case h :: t =>
      hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
    hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](
    implicit
    gen: Generic.Aux[A, R],
    rEncoder: Lazy[CsvEncoder[R]] // wrap in Lazy
  ): CsvEncoder[A] = createEncoder { value =>
    rEncoder.value.encode(gen.to(value))
  }

  println(CsvEncoder[Tree[Int]])
}
