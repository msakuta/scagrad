package example

object Hello extends App {
  var tape = new Tape
  var a = tape.value("a", 123)
  var b = tape.value("b", 123)
  var c = tape.value("c", 42)
  var ab = a + b
  var abc = ab * c
  println(abc.eval)
  println(abc.derive(a))
}

class Tape {
  var terms: Array[Term] = Array()

  def value(id: String, v: Double) = {
    val idx = terms.length
    terms = terms :+ (Value(id, v))
    TapeTerm(idx, this)
  }

  def eval_int(term: Int): Double = terms(term) match {
    case Value(_, v) => v
    case Add(lhs, rhs) => eval_int(lhs) + eval_int(rhs)
    case Mul(lhs, rhs) => eval_int(lhs) * eval_int(rhs)
  }

  def derive_int(term: Int, wrt: Int): Double = terms(term) match {
    case Value(_, _) => if (wrt == term) 1 else 0
    case Add(lhs, rhs) => derive_int(lhs, wrt) + derive_int(rhs, wrt)
    case Mul(lhs, rhs) => derive_int(lhs, wrt) * eval_int(rhs) + eval_int(lhs) * derive_int(rhs, wrt)
  }
}

case class TapeTerm(idx: Int, tape: Tape) {
  def eval(): Double = {
    tape.eval_int(idx)
  }

  def derive(wrt: TapeTerm): Double = {
    tape.derive_int(idx, wrt.idx)
  }

  def +(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ Add(this.idx, other.idx)
    TapeTerm(idx, tape)
  }

  def *(other: TapeTerm) = {
    val idx = tape.terms.length
    tape.terms = tape.terms :+ Mul(this.idx, other.idx)
    TapeTerm(idx, tape)
  }
}

sealed trait Term
case class Value(id: String, a: Double) extends Term
case class Add(lhs: Int, rhs: Int) extends Term
case class Mul(lhs: Int, rhs: Int) extends Term

