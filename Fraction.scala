import scala.collection.mutable.ArrayBuffer

class Fraction(val n: Int, val d: Int) //extends Ordered[Fraction]
{
  import Fraction._

  private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n,d)
  private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n,d)

  override def toString: String = num + "/" + den

  def sign(a: Int): Int = if (a > 0) 1 else if (a < 0) -1 else 0
  def gcd(a: Int, b: Int): Int =  if (b == 0) math.abs(a) else gcd(b, a % b)
  def *(other:Fraction) = new Fraction(num * other.num, den * other.den)

  val uniq = uniqueNumber()
  def bar = foo

  //override def compare(that: Fraction): Int = den.compareTo(that.den)
}

object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)
  private def uniqueNumber() = {}
  private def foo = "2"
  def smaller[T](a:T, b:T)(implicit order : T => Ordered[T]): T = if (a < b) a else b

  def coressponds[T1, T2](s1:Seq[T1], s2:Seq[T2])(p:(T1, T2) => Boolean): Boolean = {
    for(i <- s1.indices) if (!p(s1(i), s2(i))) return false
    true
  }

  def coresspondsExt[T1, T2](s1:Seq[T1], s2:Seq[T2], p:(T1, T2) => Boolean): Boolean = {
    for(i <- s1.indices) if (!p(s1(i), s2(i))) return false
    true
  }

}

trait Corresponding[A] extends ArrayBuffer[A] {

  def coresspondsRich[B](that:Seq[B])(p:(A, B) => Boolean): Boolean = {
    for(i <- indices) if (!p(this(i), that(i))) return false
    true
  }

  def coresspondsBool[B](that:Seq[B])(p:(A, B) => Boolean): IndexedSeq[(A, Boolean, String)] = {
    for(i <- indices) yield (this(i), p(this(i), that(i)), this(i) + "<=>" + that(i))
  }

  def coresspondsRichWC[B](that:Seq[B], p:(A, B) => Boolean): Boolean = {
    for(i <- indices) if (!p(this(i), that(i))) return false
    true
  }

}

object runFraction extends App {
  //  private val fraction: Fraction = Fraction(3, 4)
  private val fraction: Fraction = Fraction(0, 0)
  private val fraction2 = Fraction(2, 5)
  val result  = fraction * fraction2
  println(result)
  println("SMALLER " + Fraction.smaller(fraction, fraction2)((thisFraction: Fraction) => (that: Fraction) => thisFraction.d.compareTo(that.d))) //manual implicit convers
  //  fraction.>=(fraction2)
  Fraction.smaller("Hello", "World")

  val s1 = ArrayBuffer("Hello", "FooBar")
  val s0 = new ArrayBuffer[String] with Corresponding[String]
  s0.append("Hello", "Foo Bar")
  val s2 = ArrayBuffer(5, 6)

  val cor4 = s0.coresspondsRich(s2)(_.length == _)
  //  val cor4 = s0.coresspondsRichWC[Int](s2, (a, b) => a.length == b)
//  val cor44 = s0.coresspondsRichWC(s2, (a : String, b : Int) => a.length == b)
  val cor44 = s0.coresspondsRichWC[Int](s2, _.length == _)
  println("CORR 4 " + cor4)
  println("CORR 44 " + cor44)

  def corespBoolCurried: ((String, Int) => Boolean) => IndexedSeq[(String, Boolean, String)] = s0.coresspondsBool(s2)

  val c = s0.coresspondsRichWC[Int] _
  val cc = c.curried
  cc.toString()
  println(" c.curried " + cc(s2))

//  val cccc : s0.coresspondsRichWC[Int].curried


  println("Curry " + corespBoolCurried)
  val cro45 = corespBoolCurried(_.length == _)
  println(" YIELD " + cro45)
  println(" YIELD " + cro45.map(_.getClass))

  val cor = Fraction.coressponds[String, Int](s1, s2)(_.length == _)
  //  val cor = Fraction.coressponds[String, Int](s1, s2)((a, b) => a.length == b)
  println("CORR " + cor)

  val cor2 = Fraction.coresspondsExt[String, Int](s1, s2, _.length == _)
  println("CORR2 " + cor2)

  //  val cor3 = s1.corresponds(s2)((a, b: Int) => a.length == b)
  val cor3 = s1.corresponds(s2)(_.length == _)
  println("CORR3 " + cor3)


  case class Email(subject: String, text: String, sender: String, recipient: String)
  type EmailFilter = Email => Boolean
  type IntPairPred = (Int, Int) => Boolean
//  def sizeConstraint(pred: IntPairPred, n: Int, email: Email) = pred(email.text.length, n)
  val ge: IntPairPred = _ >= _
  val le: IntPairPred = _ <= _

  def sizeConstraint(pred: IntPairPred)(n: Int)(email: Email): Boolean = pred(email.text.length, n)
  val sizeConstraintFn: IntPairPred => Int => Email => Boolean = sizeConstraint _
  val minSize: Int => Email => Boolean = sizeConstraint(ge)
  val maxSize: Int => Email => Boolean = sizeConstraint(le)

  val gt: IntPairPred = _ > _
  val lt: IntPairPred = _ < _
  val eq: IntPairPred = _ == _


  case class User(name: String)
  trait EmailRepository {
    def getMails(user: User, unread: Boolean): Seq[Email]
  }
  trait FilterRepository {
    def getEmailFilter(user: User): EmailFilter
  }
  trait MailboxService {
    def getNewMails(emailRepo: EmailRepository)(filterRepo: FilterRepository)(user: User) =
      emailRepo.getMails(user, unread = true).filter(filterRepo.getEmailFilter(user))
    val newMails: User => Seq[Email]
  }

  object MockEmailRepository extends EmailRepository {
    def getMails(user: User, unread: Boolean): Seq[Email] = Nil
  }
  object MockFilterRepository extends FilterRepository {
    def getEmailFilter(user: User): EmailFilter = _ => true
  }
  object MailboxServiceWithMockDeps extends MailboxService {
    val newMails: (User) => Seq[Email] = getNewMails(MockEmailRepository)(MockFilterRepository)
  }

  println(MailboxServiceWithMockDeps.newMails(User("daniel")))

}
