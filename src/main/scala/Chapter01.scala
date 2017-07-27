object Chapter01 extends App {
  case class Employee(name: String, number: Int, manager: Boolean)
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  def employeeCsv(e: Employee): List[String] =
    List(e.name, e.number.toString, e.manager.toString)

  def iceCreamCsv(c: IceCream): List[String] =
    List(c.name, c.numCherries.toString, c.inCone.toString)

  import shapeless._

  val genericEmployee = Generic[Employee].to(Employee("Dave", 123, false))

  val genericIceCream = Generic[IceCream].to(IceCream("Sundae", 1, false))

  def genericCsv(gen: String :: Int :: Boolean :: HNil): List[String] =
    List(gen(0), gen(1).toString, gen(2).toString)

  println(genericCsv(genericEmployee))
  println(genericCsv(genericIceCream))
}
