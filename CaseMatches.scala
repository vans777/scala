import scala.annotation.tailrec

object CaseMatches extends App {

  //CASE 01.03.2017, chapter14

  //  println(swap((5, 9)))
  //  println(swap(("A", "z")))
  println()
  println(swapArr(Array(1, 2, 3, 4)).mkString(", "))
  println(swapArr(Array("A", 2, 3, 4)).mkString(", "))
  println(swapArr(Array("A")).mkString(", "))
  println(swapArr(Array("A", 2)).mkString(", "))

  def swap(tuple2: (Any, Any)): (Any, Any) = {
    tuple2 match {
      case (x, y) => Tuple2(y, x)
    }
  }

  def swapArr(array: Array[Any]): Array[Any] = {
    array match {
      case Array(x, y) => Array(x, y)
      case Array(x, y, rest@_*) => Array(y, x) ++ rest
      case rest@_ => Array() ++ rest
    }
  }

  def summ(usarray: List[Int]): Int = {
    usarray match {
      case h :: t => h + summ(t)
      case Nil => 0
    }
  }

  val nestedList = List(1, 2, List(4, List(5, 6, 7, 8, List(1))))
  val sumRes = sumAndDetectAny(List(List(3), 1, 2, nestedList, 3, "One", "100", 500L, 50.0, Unit, 2, 5))
//  nestedList.reduceLeft(detectAny(_))
  //  val sumRes = sumAndDetectAny(List(1, 2, 3, "One", "100", 500L, 50.0, Unit, List(List(1), 1, 2, List(4, List(5, 6, 7, 8)))))
  println()
  println(sumRes)

  def sumAndDetectAny(anyArray: List[Any]): Int = {
    anyArray match {
      case Nil => 0
      case h :: t =>
        h match {
          case l: List[Any] => println("List[Any]"); sumAndDetectAny(l) + sumAndDetectAny(t)
          case x: Int => print(x + " Int "); x + sumAndDetectAny(t)
          case d: Double => d.asInstanceOf[Int] + sumAndDetectAny(t)
          case s: String => print(s + " String "); sumAndDetectAny(t)
          case unn@_ => print(unn + " UNKNN "); sumAndDetectAny(t)
        }
    }
  }

  @tailrec
  def detectAny(anyArray: List[Any]): Int = {
    anyArray match {
      case Nil => 0
      case h :: t =>
        h match {
          case x: Int => print(x + " Int "); detectAny(t)
          case s: String => print(s + " String "); detectAny(t)
          //          case l: List[Any] => println("List[Any]"); detectAny(l) + detectAny(t)
          //          case h::t2 => println("\n" + h + " List " + t);  detectAny(t2)
          case unn@_ => print(unn + " UNKNN "); detectAny(t)
        }
    }
  }

  testTree()

  def testTree(): Unit = {
    val f = (x : Int, y : Int) => x - y
    val node = ExtNode(f, Leaf(3), Leaf(8))
    val f2 = (x : Int, y : Int) => x + y
    val en = ExtNode(f2, node, Leaf(2), ExtNode(f, Leaf(5), ExtNode(f,Leaf(2), Leaf(2))))



    println("Leaf Sum = " + VarArgsTree.leafSum(f, en))

  }



  //moneyTrain()
  // 27.05
  def moneyTrain() = {

    import Money._
    //page 168
    //    val aa = Money(5, 98) + Money(9, 45)
    val aa = Money(5, 98) + Money(9, 45)
    val bb = Money(5, 98) == Money(5, 98)
    val cc = Money(5, 98) < Money(5, 100)
    val dd = new Money("902")
    val ee = new Money(BigDecimal("5.5"))
    println("--")
    println(dd)
    println(ee)
    //    val dd = Money(5, 98) > Money(5, 100)
    println(aa)
    println(bb)
    println(cc)
    println("--")
    assert(Money(1, 75) + Money(0, 50) == Money(2, 25))
    //    var money = Money(1, 75)
    //    val (dols : Int, cents : Int) = Money.unapply(money).getOrElse("No Money, No Honey!")
    //    println(dols + "..." + cents)
    //    val (dols : Int, cents : Int) = Money(2, 3)
    println("EXTRACTOR")
    val mon = new Money("10.546")
    mon match {
      case Money(a) => val (dols: Int, cents: Int) = a; println(dols + "$" + cents);
      case _ => println("No matches found")
    }
  }

}
