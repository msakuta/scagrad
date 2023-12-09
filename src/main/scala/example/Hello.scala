package example

object Hello extends App {
  def eval(term: Term): Double = term match {
    case Value(_, v) => v
    case Add(_, lhs, rhs) => eval(lhs) + eval(rhs)
    case Mul(_, lhs, rhs) => eval(lhs) * eval(rhs)
  }

  def derive(term: Term, wrt: Term): Double = term match {
    case Value(id, _) => if (wrt == term) 1 else 0
    case Add(_, lhs, rhs) => derive(lhs, wrt) + derive(rhs, wrt)
    case Mul(_, lhs, rhs) => derive(lhs, wrt) * eval(rhs) + eval(lhs) * derive(rhs, wrt)
  }

  var a = Value("a", 123)
  var b = Value("b", 123)
  var c = Value("c", 42)
  var ab = Add("ab", a, b)
  var abc = Mul("abc", ab, c)
  println(eval(abc))
  println(derive(abc, a))
}

sealed trait Term
case class Value(id: String, a: Double) extends Term
case class Add(id: String, lhs: Term, rhs: Term) extends Term
case class Mul(id: String, lhs: Term, rhs: Term) extends Term

