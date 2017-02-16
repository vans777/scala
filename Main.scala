import java.awt.geom.Ellipse2D
import java.time.format.DateTimeFormatter
import java.time._
import java.util.Date

//import akka.actor.{ActorSystem, Props}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Numeric.{ShortIsIntegral, FloatIsFractional, IntIsIntegral, DoubleIsFractional}
import scala.reflect.ClassTag


object Main extends App {

  //  val system = ActorSystem("MySystem")
  //  val myActor = system.actorOf(Props[MyActor], name = "myactor")

  //learnSeq
  //  zipTrain
//  othTrain
//  curryTrain
  instantTrain




  def learnSeq(): Unit = {
    val pairs = (1 to 10) zip (11 to 20)
    println(" ee  ")
    println(pairs)
    //    val rr = pairs.map(adjustToPair)
    //    println(rr)
    //    val r = adjustToPair(pairs.tail.head)
    //    println(r)
    val j: String =  55 toString
    val pv = scala.collection.immutable.Vector.empty


    val arrayOfInt = Array (1, 2, 3, 4, 6, 95666666666666L, 1, 2, 3)
    println("REDUCING")
    println(arrayOfInt.reduceLeft((a,v) => if (a < v)  v else a))
    println(arrayOfInt.reduceRight((a,v) => if (a < v)  v else a))
    println("----------------")
    println(arrayOfInt.reduceRight(comp))
    println("----------------")
    println(arrayOfInt.reduceLeft(comp))
    println("----------------")
    println(arrayOfInt.reduce(comp))


    println("FOLDING")
    val namesList = List("Daniel", "Chris", "Joseph")
    val str = namesList.reduceLeft((acc, n) => acc + ", " + n)
    val flatMapRes = namesList.flatMap(f => f)
    val mapRes = namesList.map(f => f)

    val indexes = new mutable.HashMap[Char, Int]()
    val misString: String = "Mississippi"
    misString.flatMap(ch => indexes.put(key = ch, value = misString.indexOf(ch)))
    //    misString.toCharArray.foreach(c => misString.zipWithIndex)
    val frequency = new mutable.HashMap[Char, mutable.TreeSet[Int]]()

    println("------FREQUENCIES----- ")
    misString.zipWithIndex.foreach{case(c, i) =>  if (frequency.contains(c))  { frequency.get(c).get.+=(i) } else  frequency.put(c, new mutable.TreeSet[Int]().+=(i) )}
    //    frequency.reduce{ (a, b) => a + " - " + b }
    //    val strr = frequency.fold((a, b) => a + "-" + b)
    val strr = frequency.fold(0)(_ + " " +  _)
    var concatenated2: String = ""

    for(fr <- frequency) {
      //      val left: String = fr._2.reduceLeft((a, b) => a.toString.concat(b.toString))
      println(fr._1 + " - {"  + fr._2.mkString(", ") + "}")
      concatenated2 += fr._2.tail.foldLeft(new StringBuilder(fr._2.head)) { (acc, e) => acc.append(", ").append(e) } //TODO: DESIRED
    }

    println("----------------concatenated2-------------------")
    println(concatenated2)

    //from book
    val freqBook = scala.collection.mutable.Map[Char, Int]()
    for(c <- "Mississippi") freqBook(c) = freqBook.getOrElse(c, 0) + 1

    //    frequency.foreach(_._1 + " - " + _._2)
    println(strr)
    println(frequency)

    //    misString.zipWithIndex.foreach( => print(c))
    println()

    println("INDEXES " + indexes)

    println(str)
    println(flatMapRes)
    println(mapRes)

    //yielding()

  }

  def comp (a: Long, b: Long): Long = {
    //println("a=" + a + " b=" + b + " ")
    if (a < b)  b else a
    //      a-b
  }

  def yielding () : Unit = {
    val fr = new ListBuffer[Int]()
    for(i <-(0 until 100).par) print(i + " ")
    println()
    println("--------ParVec--------")
    for (i <- (0 until 100).par) yield fr += i
    //    pv = pv :+ 1 :+ 5
    Thread.sleep(500)
    println(fr)
    print(for(i <-(0 until 100).par) yield i + " ")
    println()
    for(i <-(0 until 100).par) yield (i + " ")
  }

  //  def adjustToPair1(p1: Int, p2:Int => Int) {
  ////    p1.+(p2.asInstanceOf)
  //        p1 + p2
  //  }

  def adjustToPair2(fun : (Int, Int)): Int = {
    fun._1 * fun._2
  }

  def newInstance[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass.newInstance.asInstanceOf[T]

  class Foo[T](implicit ctag: reflect.ClassTag[T]) {
    def foo: Class[T] = ctag.runtimeClass.asInstanceOf[Class[T]]
  }

  def adjustToPair4[T, T2, T3](fun : (T, T2)) (implicit num: Numeric[T], num2: Numeric[T2], num3: Numeric[T3]): T3 =
  {
    //    println("num " + num.toString + " num2 " + num2.toString + " num3 " + num3.toString)
    //val z = new java.lang.Double(num3.toDouble())

    (num3 match {
      case ff: FloatIsFractional => {
        println("FloatIsFractional")
        num.toFloat(fun._1) * num2.toFloat(fun._2)
        //        println("FloatIsFractional 2")
      }
      case df: DoubleIsFractional => {
        println("DoubleIsFractional")
        num.toDouble(fun._1) * num2.toDouble(fun._2)
        //        println("DoubleIsFractional 2")
      }
      case ii: IntIsIntegral => {
        println("IntIsIntegral")
        val ff = num.toInt(fun._1) * num2.toInt(fun._2)
        println(ff + " " + ff.getClass)
        ff.toInt
        //        println("IntIsIntegral 2")
      }
      case  _ => {

        //        newInstance[ClassTag[T3]]

        //        println("case _ ")
        //        num3

      }
    }).asInstanceOf[T3]
    //    println("after match " + res + "  " + res.getClass)
    //    println(res.asInstanceOf[BoxedUnit].toString)
    //    num3.erasure.newInstance.asInstanceOf[T3]

    //    res.asInstanceOf[T3]

  }

  //22.05
  def zipTrain()  = {
    //    val prices = List(5.0, 20.0, 9.95)
    val prices = List(5, 20, 9)
    val quantities = List(10, 2, 1)
    //    val quantities = List(10.0, 2.0, 1.0)
    val myf = new (
      (Double, Int) => Double) {
      override def apply(v1: Double, v2: Int): Double = v1*v2
    }
    //    val res = (prices zip quantities) map { myf.tupled}


    val res = (prices zip quantities) map adjustToPair4[Int, Int, Float]

    //    Function.tupled()

    val a = "gfgdfg"
    val b = 7.0

    println(res)
  }

  // 27.05
  def othTrain = {
    //    val ar1 = "Hello" -> 42
    //    val ar2 = 42 -> "Hello"
    //    println(ar1);
    //    println(ar2);
    //    println(ar1.getClass)
    //    println(ar2.getClass)

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
    val x: Boolean = Money(1, 75) + Money(0, 50) == Money(2, 25)
    println(x)
    //    var money = Money(1, 75)
    //    val (dols : Int, cents : Int) = Money.unapply(money).getOrElse("No Money, No Honey!")
    //    println(dols + "..." + cents)
    //    val (dols : Int, cents : Int) = Money(2, 3)
    println("EXTRACTOR")
    val mon = new Money("10.546")
    mon match {case  Money(a) =>  val (dols : Int, cents : Int) = a; println(dols + "$" + cents)
    case _ => println("No matches found")}


    //    case money(d, c) => println(d + "$" + c)
    println("abc".map(_ toUpper))
    "abc".map(_ toInt).foreach(println)

//    println('a'.toInt)

    //    println(aa.toString.split('.').toList) //.foreach(println)
  }

 def instantTrain: Unit = {
   val t1 = Instant.now
   println(tailRecursionFactorial(100000, 1).toString().length)
   val t2 = Instant.now()
   //  Period.between(t1, )
   //  LocalDateTime.now()
   val t = Duration.between(t1, t2)
   println(" Factorial calculating time = " + t)

   println("PartialFunction")
   val f: PartialFunction[Char, Int]= { case '+' => 1; case '-' => -1; }
   println(f('-'))
   //println(f('u')) //todo: Exception
   println(f.isDefinedAt('0'))

   //  println(f('0'))
   val col = "-3+4".collect { case '+' => 1; case '-' => -1}
   println(col)

   //Traits 01.06.2016
   val egg = new Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
   //  egg.translate(10, -10)
   //  egg.grow(10, 20)
   //  egg.getHours
   //  egg.createIntersection()
 }


  @tailrec def tailRecursionFactorial(x : BigInt, res : BigInt) : BigInt = {
    if(x > 1) tailRecursionFactorial(x - 1, res * x)
    else res
  }

  def workWithDate() : Unit = {
    //  val tmp: Temporal
    println("Instant.now " + Instant.now)
    println("LocalDateTime.now " + LocalDateTime.now)
    var d : LocalDate = LocalDate.now()
    println("d0 = " + d)
    println("startOfDay " + d.atStartOfDay())
    val fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    d = d.minusMonths(4)
    println("startOfDay formatted " + fmt.format(d.withDayOfMonth(1)))
    println("startOfDay formatted " + fmt.format(d.withDayOfMonth(d.getMonth.maxLength())))
    val date: YearMonth = YearMonth.parse("2016.06", DateTimeFormatter.ofPattern("yyyy.MM"))
    println("parsed " + fmt.format(date.atDay(1)))
    println("parsed " + fmt.format(date.atEndOfMonth()))
    val dasd : Date = new Date()
    println("d = " + dasd)
  }

  //08.08
  def curryTrain: Unit = {
    def noo(x:Int, y:Int) = x * y
    val a = noo _ // получили функцию от 2-х аргументов (подробнее см. partially applied function)
    val aa  = a(5, _:Int)

    println(aa(7))
    val b = a.curried
    println(b(2)) // = функция 2 * аргумент
    println(b(2)(3)) // = 6
    val c = a.curried(5)
    println(c(5))

    val aar = Array("Hello", "World")
    val abr = Array("Hello", "World")
    val abq = Array(5, 5)
    println("CORRESPONDS")
    println(aar.corresponds(abq)(_.length == _))


  }


  println("largest")
  largest(c => 10*c - c*c, 1 to 10)
  largestAt(x => 10*x - x*x, 1 to 10)
  println("largest At DOuble")
  largestAtDouble((x,y) => x*y, mutable.Seq((1,2), (3,4)))

  //09.08
  def largest(fun: (Int) => Int, inputs: Seq[Int]): Unit = {
    println(inputs.map(fun).max)
    println("MAXX BY")
    val indexWhere: Int = inputs.indexWhere(fun(_) == inputs.map(fun).max)
    println(inputs(indexWhere))
  }


  def largestAt(fun: (Int) => Int, inputs: Seq[Int]): Unit = {
    println(inputs.map(fun).zip(inputs).max._2)
  }

  def largestAtDouble(fun: (Int, Int) => Int, inputs: Seq[Tuple2[Int, Int]]): Unit = {
    println(inputs.map{case(a:Int, b:Int) => fun(a,b)}.max)
    println(inputs.map{t => fun(t._1, t._2)}.min)
    //map tupled
  }


  println("CURRY")
  curryTest

  def curryTest  {

    def filter(xs: List[Int], p: Int => Boolean): List[Int] =
      if (xs.isEmpty) xs
      else if (p(xs.head)) xs.head :: filter(xs.tail, p)
      else filter(xs.tail, p)

    def modN(n: Int)(x: Int) = ((x % n) == 0)

    val nums = List(1, 2, 3, 4, 5, 6, 7, 8)

    println(filter(nums, modN(2)))
    println(filter(nums, modN(3)))

  }


  def harryHacker: Unit = {
    val frequencies = new mutable.HashMap[Char, Int]() with mutable.SynchronizedMap[Char, Int]
    val c = 'd'
    frequencies(c) = frequencies.getOrElse(c, 0) + 1
  }



  def values(fun: (Int) => Int, low: Int, high: Int): Unit = {
    low.to(high).zip(low.to(high).map(fun))
  }

  println("Fact with Reduce " + 1.to(6).product) //TODO: instead of reduceLeft(_*_)
//  println("Fact with Reduce " + 1.to(6).reduceLeft(_*_))
  println("Fact with Reduce " + -1.to(6).foldLeft(1)(_*_))


  //  def holesTrain = {
  //    var cont : (Unit => Unit) = null
  //    reset {
  //      println("Before shift")
  //      shift {
  //        k: (Unit => Unit) => {
  //          cont = k
  //          println("Inside shift")
  //        }
  //      }
  //      println("After shift")
  //    }
  //    println("After reset")
  //    cont()
  //  }

}