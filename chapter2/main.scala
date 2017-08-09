trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

val andMonoid = new Monoid[Boolean]{
  def empty =  false
  def combine(x: Boolean, y: Boolean) = x && y
}


case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid = new Monoid[Order]{
  def empty = Order(0,0)
  def combine(a: Order, b: Order) =
    Order(
      a.totalCost + b.totalCost,
      a.quantity + b.quantity
    )
}