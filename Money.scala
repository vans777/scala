 class Money(val currencyValue : BigDecimal) {

  def +(other : Money) =  {
    new Money(currencyValue + other.currencyValue)
  }

  def -(other : Money) =  {
    new Money(currencyValue - other.currencyValue)
  }

  def ==(other : Money) =  {
    currencyValue.compare(other.currencyValue) == 0
  }

  def < (other : Money) =  {
    currencyValue.compare(other.currencyValue) == -1
  }

  def > (other : Money) =  {
    currencyValue.compare(other.currencyValue) == 1
  }

  override def toString: String = { currencyValue.toString }


}
object Money {

  //    def apply(tbd : BigDecimal) = new Money(tbd)
  def apply(dollars: Int, cents : Int) = new Money(BigDecimal(dollars + "." + cents))
  def unapply(input: Money) = if (input != null) Some(input.toString.split('.').toList.head.intValue(), input.toString.split('.').toList.tail.head.intValue()) else None
  implicit def string2BigDecimal(s : String) : BigDecimal = BigDecimal(s)

}


