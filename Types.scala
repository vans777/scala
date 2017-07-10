import scala.collection.mutable.ArrayBuffer

object Types extends App {

  class Network {
    class Member(val name: String) {
      val contacts = new ArrayBuffer[Network#Member]
    }

    private val members = new ArrayBuffer[Network#Member]

    def join(name: String): Member = {
      val m = new Member(name)
      members += m
      m
    }
  }

  def process[M <: n.Member forSome {val n: Network}] (m1: M, m2: M): (M, M) = (m1, m2)

  def appendLines(target: {def append(str: String) : Any}, lines: Iterable[String] ): Unit = {
    for (l <- lines) {
      target.append(l) // todo: Reflective call
      target.append("\n")
    }
  }

  val chatter = new Network
  val myFace  = new Network
  val bot1 = chatter.join("BOT 1")
  val bot2 = chatter.join("BOT 2")
  val bot3 = myFace.join("BOT 3")
  println(process(bot1, bot2))
  //  process(bot1, bot3)

  //  Range(1, 1000).foreach(Range(_, 1000).foreach(println(_)))

  def for1000() : Unit = {
    for (i <- 1 to 1000; j <- 1 to 1000 if i != j) {
      for (k <- j to 300 if j != k; l <- k to 300 if l != k) {
        if (i * i + j * j == k * k + l * l) {
          println(i, j, k, l)
        }
      }
    }
  }

  //self types 08.06.2017

  abstract class Dim[T](val value: Double, val name: String) {
    this: T =>
    protected  def create(v: Double): T
    def +(other: Dim[T]): T = create(value + other.value)
    override def toString: String = value + " " + name
  }

  class Seconds(v: Double) extends Dim[Seconds](v, "s") {
    override protected def create(v: Double): Seconds = new Seconds(v)
  }

  //  class Meters(v: Double) extends Dim[Meters](v, "m") {
  //    override protected def create(v: Double): Seconds = new Seconds(v)
  //  }

  val s5 = new Seconds(5)
  //  val m7 = new Meters(7)
  //  println("s+m ", s5 + m7)


  def printValues (f: { def apply(x: Int): Int}, from: Int, to: Int) : Unit = { // todo: by reflect, expensive
    from.to(to).map(f.apply).foreach(println(_))
  }

  //  def function: (x: Int): Int => x * x
  protected val function = new Function[Int, Int] {
    override def apply(x: Int): Int = x*x
  }
  private val array = Array[Int] (1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  //
  printValues(array,3,6)
  println("-----")
  printValues(function, 3, 6)

  //  for1000

  def summarize(a : Integer, b : Integer) = {
    a + b
  }

  //  case class Person() extends Friend[Person] {
  //    override def befriend(someone: Person): Unit = { println(this, someone) }
  //  }
  //  class Student() extends Person {}
  //
  //
  //  class Pair[+T](val first : T, val second : T)
  //
  //  def makeFriends(p : Pair[Person]) = {}
  //
  //  makeFriends(new Pair[Person](Person, Person))
  //  makeFriends(new Pair[Student](new Student, new Student))
  //
  //  trait Friend[-T] {
  //    def befriend(someone: T)
  //  }
  //
  //  def makeFriendsWith(s: Student, f: Friend[Student]) { f.befriend(s) }
  //
  //  val susan = new Student
  //  val fred  = new Person
  //  makeFriendsWith(susan, fred)


  //29.06
  abstract class Type [-T]{
    def typeName : Unit
  }

  class SuperType extends Type[AnyVal]{
    override def typeName: Unit = {
      println("SuperType")
    }
  }

  class SubType extends Type[Int]{
    override def typeName: Unit = {
      println("SubType")
    }
  }

  class SubAnyType extends Type[Any]{
    override def typeName: Unit = {
      println("SubType Any")
    }
  }

  class TypeCarer{
    def display(t: Type[Int]){
      t.typeName
    }
  }


  val superType = new SuperType
  val subType = new SubType
  val subAnyType = new SubAnyType

  val typeCarer = new TypeCarer

  typeCarer.display(subType)
  typeCarer.display(subAnyType)
  typeCarer.display(superType)


  class SlowHeadQueue[T](elems: List[T]) {
    def head = elems.head
    def tail = new SlowHeadQueue[T](elems.tail)
    def enqueue(x: T) =  new SlowHeadQueue[T](elems ::: List(x))
  }

  trait Queue[T] {
    def head: T
    def tail: Queue[T]
    def enqueue(x: T): Queue[T]
    def printLeadTrail():Unit
  }

  object Queue {

    def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)

    private class QueueImpl[T](private val leading: List[T], private val trailing: List[T]) extends Queue[T] {

      private def mirror = {
        if (leading.isEmpty)
          new QueueImpl(trailing.reverse, Nil)
        else
          this
      }

      def head: T = mirror.leading.head

      def tail: QueueImpl[T] = {
        val q = mirror
        new QueueImpl(q.leading.tail, q.trailing)
      }

      def enqueue(x: T) = new QueueImpl(leading, x :: trailing)

      override def toString: String = (leading ::: trailing.reverse).mkString(",")
      def printLeadTrail(): Unit = {
        println("Leading ",  leading.mkString(","))
        println("Trailing ", trailing.mkString(","))
      }
    }

  }

  val q = Queue[Int](1,2,3,4)
  println(q)
  q.printLeadTrail()
  println(q.tail)
  val q2 = q.enqueue(8)
  println(q2)
  q.printLeadTrail()
  q2.printLeadTrail()

  //  class Cell[+T](init: T) {
  //    private[this] var current = init
  //    def get = current
  //    def set(x: T) = { current = x }
  //    override def toString: String = current.toString
  //  }
  //
  //  val a  = new Cell[String]("e")
  //  val a2: Cell[Any] = a
  //  a2.set(15)
  //  a2.get
  //  println(a2)
  //  a.set("")

  class Pair[+T](val first: T, val second: T) {
    def replaceFirst[U >: T](newFirst: U) = new Pair[U](newFirst, second)
  }

  class PairImmutable[T](val first: T, val second: T) {
    def replaceFirst(newFirst: T) = new PairImmutable[T](newFirst, second)
//    def replaceFirst[U >: T](newFirst: U) = new PairImmutable[U](newFirst, second)
    override def toString: String = first + " " + second
  }

  class NastyDoublePair(first: Double, second: Double) extends PairImmutable[Double](first, second) {
//    def replaceSecond4(newSecond: Double): Unit = new NastyDoublePair(first, Math.sqrt(newSecond))
//    override def replaceFirst(newFirst: Double): NastyDoublePair =  new NastyDoublePair(Math.sqrt(first), second)
    override def replaceFirst(newFirst: Double): NastyDoublePair = new NastyDoublePair(newFirst, second)
//        NastyDoublePair(Math.sqrt(first), second)
  }

  val n = new NastyDoublePair(1, 4)
//  val pm: PairImmutable[Any] = new PairImmutable[Double](1,2)
//  val replaced = pm.replaceFirst("Hello")
//  println(pm)
//  println(replaced)

  class PairBi[F, S](var first: F, var second: S) {
    def swap(implicit ev: F =:= S): Unit = {
      var third = first
      first = second.asInstanceOf[F]
      second = third
    }
    override def toString: String = first + " " + second
  }

//  val pbInt = new PairBi[Int, Int](1,2)
  val pbInt = new PairBi[Int, String](1,"BBB")
  println(pbInt)
  pbInt.swap
  println(pbInt)



}
